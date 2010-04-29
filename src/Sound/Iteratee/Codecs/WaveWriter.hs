{-# LANGUAGE ScopedTypeVariables #-}

module Sound.Iteratee.Codecs.WaveWriter (
  -- * Types
  WaveCodec (..),
  -- * Writing functions
  -- ** Writing iteratees
  writeWave,
  -- ** Low-level writing functions
  openWave,
  closeWave,
  runWaveAM,
  writeFormat,
  writeDataHeader,
  writeDataChunk
)
where

import Sound.Iteratee.Base
import Data.MutableIter
import qualified Data.MutableIter.IOBuffer as IB
import Data.MutableIter.IOBuffer (IOBuffer, hPut, mapBuffer)
import qualified Data.Iteratee as I
import Data.Int.Int24
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Char8 as BC
import qualified Data.Binary.Put as P
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Foreign

import System.IO

import GHC.Float

-- =====================================================
-- WAVE libary code

-- |Data type to specify WAVE-formatted data
data WaveCodec = WaveCodec

instance WritableAudio WaveCodec where
  emptyState         WaveCodec   = WaveState Nothing Nothing 0 0 0
  initState          WaveCodec h = WaveState (Just h) Nothing 0 0 0
  supportedBitDepths WaveCodec   = Supported [8,16,24,32]
  fileType           WaveCodec   = Wave

-- ---------------------
-- Functions to support writing

-- |Create an iteratee to write data to a wave file.
writeWave ::
  FilePath
  -> AudioFormat
  -> MIteratee (IOBuffer r Double) AudioMonad ()
writeWave fp af = do
  lift $ openWave fp
  lift $ writeFormat af
  lift writeDataHeader
  offp <- liftIO $ newFp 0
  bufp <- liftIO (mallocForeignPtrArray defaultChunkLength
            :: IO (ForeignPtr Word8))
  loop offp bufp
  lift closeWave
  lift $ put NoState
  where
    loop offp bufp = liftI (step offp bufp)
    step offp bufp (I.Chunk buf) = guardNull buf (loop offp bufp) $ do
      lift $ writeDataChunk offp bufp buf
      loop offp bufp
    step _ _ stream      = idone () stream

-- |Open a wave file for writing
openWave :: FilePath -> AudioMonad ()
openWave file = do
  h <- liftIO $ openFile file WriteMode
  liftIO $ LB.hPut h . P.runPut $ writeTopHeaderRaw
  let (WaveState h' f i i' off) = initState WaveCodec h
  put $ WaveState h' f (i + 4) i' (off + 12)
  
-- |Write a data format block to the open wave file
writeFormat :: AudioFormat -> AudioMonad ()
writeFormat af = do
  as <- get
  case as of
    WaveState (Just h) Nothing  i i' off -> do
      liftIO $ LB.hPut h . P.runPut $ writeFormatRaw af
      put $ WaveState (Just h) (Just af) (i + 24) i' (off + 24)
    WaveState Nothing  _      _ _ _  -> error "Can't write: no file opened"
    WaveState _      (Just _) _ _ _  -> error "Format already written"
    _                                -> error "Can't write: not a WAVE file"

-- |Write the header for a Data chunk.
writeDataHeader :: AudioMonad ()
writeDataHeader = do
  as <- get
  case as of
    WaveState (Just h) (Just af) i i' off -> do
      liftIO $ LB.hPut h . P.runPut $ writeDataRaw
      put $ WaveState (Just h) (Just af) (i + 8) i' (off + 4)
    WaveState Nothing  _       _ _ _  -> error "Can't write: no file opened"
    WaveState _        Nothing _ _ _  -> error "No format specified"
    _                                 -> error "Can't write: not a WAVE file"

-- |Write a data chunk.
writeDataChunk :: (Integral a, Bounded a, Storable a) =>
  ForeignPtr Int
  -> ForeignPtr a
  -> IOBuffer r Double
  -> AudioMonad ()
writeDataChunk offp bufp buf = do
  as <- get
  case as of
    WaveState (Just h) (Just af) i i' off -> do
      len <- liftIO . liftM fromIntegral $ getLength af
      liftIO $ putVec af h buf
      put $ WaveState (Just h) (Just af) (i + len) (i' + len) off
    WaveState Nothing  _       _ _ _  -> error "Can't write: no file opened"
    WaveState _        Nothing _ _ _  -> error "No format specified"
    _                                 -> error "Can't write: not a WAVE file"
  where
    putVec af h buf' = case bitDepth af of
      8  -> convertVector af offp (myCastPtr i8  bufp) buf' >>= hPut h
      16 -> convertVector af offp (myCastPtr i16 bufp) buf' >>= hPut h
      24 -> convertVector af offp (myCastPtr i24 bufp) buf' >>= hPut h
      32 -> convertVector af offp (myCastPtr i32 bufp) buf' >>= hPut h
      x  -> error $ "Cannot write wave file: unsupported bit depth " ++ show x
    getLength af = liftM (fromIntegral (bitDepth af `div` 8) *) (IB.length buf)

i8 :: Int8
i8 = 0
i16 :: Int16
i16 = 0
i24 :: Int24
i24 = 0
i32 :: Int32
i32 = 0

myCastPtr :: a -> ForeignPtr el -> ForeignPtr a
myCastPtr _ = castForeignPtr

closeWave :: AudioMonad ()
closeWave = do
  s <- get
  case s of
    WaveState (Just h) _ i i' off -> do
      liftIO $ hSeek h AbsoluteSeek 4
      liftIO $ LB.hPut h $ P.runPut $ P.putWord32le $ fromIntegral i
      liftIO $ hSeek h AbsoluteSeek off
      liftIO $ LB.hPut h $ P.runPut $ P.putWord32le $ fromIntegral i'
      liftIO $ hClose h
    WaveState Nothing  _  _ _  _ -> error "Can't close file: no handle"
    x -> error $ "Can't close file: isn't a WAVE file: " ++ show x

runWaveAM :: AudioMonad a -> IO a
runWaveAM m = evalStateT (m >>= (\a -> closeWave >> return a))
                         (emptyState WaveCodec)
 
writeTopHeaderRaw :: P.Put
writeTopHeaderRaw = do
  P.putByteString $ BC.pack "RIFF"
  P.putWord32le 36
  P.putByteString $ BC.pack "WAVE"

writeFormatRaw :: AudioFormat -> P.Put
writeFormatRaw (AudioFormat nc sr bd) = do
  let nc' = fromIntegral nc -- Word32
      sr' = fromIntegral sr -- Word32
      bd' = fromIntegral bd -- Word32
  P.putByteString $ BC.pack "fmt "
  P.putWord32le 16
  P.putWord16le 1
  P.putWord16le $ fromIntegral nc
  P.putWord32le sr'
  P.putWord32le (sr' * nc' * (bd' `div` 8))
  P.putWord16le $ fromIntegral (nc' * (bd' `div` 8))
  P.putWord16le $ fromIntegral bd

writeDataRaw :: P.Put
writeDataRaw = do
  P.putByteString $ BC.pack "data"
  P.putWord32le maxBound

-- ------------------------------------------
-- Data normalization and conversion functions

convertVector ::
  (Integral a, Storable a, Bounded a) =>
  AudioFormat
  -> ForeignPtr Int
  -> ForeignPtr a
  -> IOBuffer r Double
  -> IO (IOBuffer r a)
convertVector (AudioFormat _nc _sr bd ) = mapBuffer (unNormalize bd)

-- 8 bits are handled separately because (at least in wave) they aren't 2's
-- complement negatives.
unNormalize :: forall a.(Integral a, Bounded a) => BitDepth -> Double -> a
unNormalize 8 a = fromIntegral $ double2Int (128 * (1 + a))
unNormalize _bd a = let
  posMult = fromIntegral (maxBound :: a)
  --input is already neg., so negMult needs to be positive to preserve sign
  negMult = abs $ fromIntegral (minBound :: a)
  in
  if a >= 0
    then fromIntegral . roundDoublePos . (* posMult) . clip $ a
    else fromIntegral . roundDoubleNeg . (* negMult) . clip $ a

clip :: Double -> Double
clip = max (-1) . min 1

roundDoublePos :: Double -> Int
roundDoublePos x = let b = double2Int x in if x - int2Double b >= 0.5
  then b+1
  else b

roundDoubleNeg :: Double -> Int
roundDoubleNeg x = let b = double2Int x in if x - int2Double b <= -0.5
  then b-1
  else b

