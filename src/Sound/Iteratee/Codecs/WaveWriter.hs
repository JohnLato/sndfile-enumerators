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
import Sound.Iteratee.Instances()
import Data.Iteratee.Base
import qualified Data.StorableVector as Vec
import qualified Data.StorableVector.Base as VB
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Char8 as BC
import qualified Data.Binary.Put as P
import Control.Monad.Trans
import Data.Word
import Data.Bits
import Foreign.Storable
import System.IO

import GHC.Float

-- =====================================================
-- WAVE libary code

-- |Data type to specify WAVE-formatted data
data WaveCodec = WaveCodec

instance WritableAudio WaveCodec where
  emptyState WaveCodec = WaveState Nothing Nothing 0 0 0
  initState WaveCodec h = WaveState (Just h) Nothing 0 0 0

-- ---------------------
-- Functions to support writing

-- |Create an iteratee to write data to a wave file.
writeWave :: FilePath ->
             AudioFormat ->
             IterateeGM Vec.Vector Double AudioMonad ()
writeWave fp af = do
  lift $ openWave fp
  lift $ showState "after open: "
  lift $ writeFormat af
  lift $ showState "after write format: "
  lift $ writeDataHeader
  lift $ showState "after write data header: "
  loop
  lift $ closeWave
  lift $ showState "after data: "
  lift $ put NoState
  where
  loop = liftI $ Cont step
  step (Chunk vec) | Vec.null vec = loop
  step (Chunk vec)                = lift (writeDataChunk vec) >> loop
  step stream                     = liftI $ Done () stream
  showState header = get >>= (\s -> liftIO $ putStrLn $ header ++ (show s))

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
writeDataChunk :: Vec.Vector Double -> AudioMonad ()
writeDataChunk vec = do
  as <- get
  case as of
    WaveState (Just h) (Just af) i i' off -> do
      let len = getLength af
      liftIO $ putVec af h vec
      let len' = fromIntegral len
      put $ WaveState (Just h) (Just af) (i + len') (i' + len') off
    WaveState Nothing  _       _ _ _  -> error "Can't write: no file opened"
    WaveState _        Nothing _ _ _  -> error "No format specified"
    _                                 -> error "Can't write: not a WAVE file"
  where
  putVec af h vec' = case bitDepth af of
    8  -> Vec.hPut h (convertVector af vec' :: Vec.Vector Word8)
    16 -> Vec.hPut h (convertVector af vec' :: Vec.Vector Word16)
    24 -> undefined -- Vec.hPut h (convertVector af vec' :: Vec.Vector Word24)
    32 -> Vec.hPut h (convertVector af vec' :: Vec.Vector Word32)
    x  -> error $ "Cannot write wave file: unsupported bit depth " ++ (show x)
  getLength af = fromIntegral (bitDepth af) * Vec.length vec

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
    _ -> error "Can't close file: isn't a WAVE file"

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

convertVector :: (Integral a, Storable a) =>
                 AudioFormat ->
                 Vec.Vector Double ->
                 Vec.Vector a
convertVector (AudioFormat _nc _sr bd ) v =
  Vec.map (fromIntegral . unNormalize bd) v

unNormalize :: BitDepth -> Double -> Int
unNormalize 8 a = double2Int (128 * (1 + a))
unNormalize bd a = let 
  posMult = fromIntegral $ (1 `shiftL` (fromIntegral bd - 1) :: Integer) - 1
  negMult = fromIntegral (1 `shiftL` (fromIntegral bd - 1) :: Integer)
  in
  case (a >= 0) of
    True -> roundDouble (posMult * clip a)
    False -> roundDouble (negMult * clip a)

clip :: Double -> Double
clip = max (-1) . min 1
{-# INLINE clip #-}

roundDouble :: Double -> Int
roundDouble x = case (x >= 0.5) of
  True -> double2Int (x + 0.5)
  False -> double2Int (x - 0.5)

