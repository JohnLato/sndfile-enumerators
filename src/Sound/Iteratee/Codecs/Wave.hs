{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

{-# OPTIONS_GHC -Wall #-}
module Sound.Iteratee.Codecs.Wave (
  -- * Types
  -- ** Internal types
  WaveCodec (..),
  WaveChunk (..),
  -- ** WAVE CHUNK types
  WaveChunkType (..),
  chunkToString,
  -- * Wave reading Iteratees
  -- ** Basic wave reading
  directWaveReader,
  riffWaveReader,
  readRiff,
  waveChunk,
  
  rawToWaveTrans,
  -- riffWaveTrans,
  -- chunkTrans,
  -- * Wave writing files
  -- ** Writing iteratees
  writeWave,
  -- ** Primitive wave writing functions
  openWave,
  closeWave,
  runWaveAM,
  writeFormat,
  writeDataHeader,
  writeDataChunk
)
where

import Prelude as P

import Sound.Iteratee.Codecs.WaveWriter
import Sound.Iteratee.Base
import Sound.Iteratee.Codecs.Common

import qualified Data.Vector.Storable as V

import           IterX
import           IterX.Parser as IterX
import           IterX.Parser.Binary
import           IterX.Fusion

import           Data.Word
import           Data.Char (chr, ord)

import           Control.Applicative
import           Control.Monad
import qualified Control.Monad.Catch as E

-- =====================================================
-- WAVE libary code

data WaveChunk =
    WaveDataChunk
      { waveCount    :: !Int  -- ^length of chunk
      , waveOffset   :: !Int  -- ^ offset to data start
      }
  | WaveFmtChunk
      { waveCount    :: !Int  -- ^length of chunk
      , waveOffset   :: !Int  -- ^ offset to data start
      , waveFormat   :: !AudioFormat
      }
  | WaveOtherChunk
      { waveCount    :: !Int  -- ^length of chunk
      , waveOffset   :: !Int  -- ^ offset to data start
      , waveOtherType :: String
      }
  deriving (Eq, Show)

-- |Standard WAVE Chunks
data WaveChunkType =
    WaveFmt            -- ^Format
  | WaveData           -- ^Data
  | WaveOther String   -- ^Other
  deriving (Eq, Ord, Show)

instance Enum WaveChunkType where
  fromEnum WaveFmt = 1
  fromEnum WaveData = 2
  fromEnum (WaveOther _) = 3
  toEnum 1 = WaveFmt
  toEnum 2 = WaveData
  toEnum 3 = WaveOther ""
  toEnum _ = error "Invalid enumeration value"

-- -----------------
-- wave chunk reading/writing functions

-- |Convert a string to WaveChunkType type
waveChunk :: V.Vector Word8 -> Maybe WaveChunkType
waveChunk vec = unsafeWaveChunk vec <$ guard (V.length vec == 4)

unsafeWaveChunk :: V.Vector Word8 -> WaveChunkType
unsafeWaveChunk vec
  | vec == V.fromList [102,109,116,32] = WaveFmt   -- "fmt "
  | vec == V.fromList [100,97,116,97]  = WaveData  -- "data"
  | otherwise = WaveOther $ map (chr . fromIntegral) vlist
  where
    vlist = V.toList vec

waveChunkType :: Monad m => IterX (V.Vector Word8) m WaveChunkType
waveChunkType = unsafeWaveChunk <$> IterX.take 4

-- |Convert a WaveChunkType to the representative string
chunkToString :: WaveChunkType -> String
chunkToString WaveFmt = "fmt "
chunkToString WaveData = "data"
chunkToString (WaveOther str) = str

-- -----------------

-- | parse the "RIFF/WAVE" headers and get the total size of the file.
riffWaveReader :: Monad m => IterX (V.Vector Word8) m Int
riffWaveReader = readRiff
    *> fmap (subtract 4 . fromIntegral) getWord32le <* readRiffWave

rawToWaveTrans :: (ExIO m, Functor m)
               => Transform' m (V.Vector Word8) NormFormattedChunk
-- rawToWaveTrans = riffWaveTrans . chunkTrans2 convTrans2
rawToWaveTrans = riffWaveTrans . chunkTrans . convTrans
{-# INLINE [1] rawToWaveTrans #-}

{-# INLINE riffWaveTrans #-}
riffWaveTrans :: E.MonadCatch m => Transform' m (V.Vector Word8) (V.Vector Word8)
riffWaveTrans = tdelimitN riffWaveReader

{-# INLINE chunkTrans #-}
chunkTrans :: E.MonadCatch m
    => Transform' m (V.Vector Word8) (RawFormattedChunk)
chunkTrans ofold@(FoldM _ ofs ofOut) =
    delimitFold4 i0 mkF finalFold
  where
    {-# INLINE finalFold #-}
    finalFold = FoldM (\_ c -> return $ Right c) (Left ofs) (either ofOut return)
    {-# INLINE mkF #-}
    mkF af = {-# SCC "chunkTrans/mkF" #-} maps (RawFormattedChunk af) ofold
    i0 = do
        af0 <- snd <$> nextFormatChunk
        t1 <- waveChunkType
        cnt <- fromIntegral <$> getWord32le
        case t1 of
            WaveData -> return (cnt,af0)
            _        -> error "chunkTrans: horrible hack needs a real impl"

{-# INLINE chunkTrans2 #-}
chunkTrans2 :: E.MonadCatch m
    => (AudioFormat -> Transform' m (V.Vector Word8) b) -> Transform' m (V.Vector Word8) b
chunkTrans2 foldFormat ofold =
    delimitFold4 i0 (\af -> foldFormat af ofold) (foldLast $ error "chunkTrans2: no audio data")
  where
    i0 = do
        af0 <- snd <$> nextFormatChunk
        t1 <- waveChunkType
        cnt <- fromIntegral <$> getWord32le
        case t1 of
            WaveData -> return (cnt,af0)
            _        -> error "chunkTrans: horrible hack needs a real impl"


-- the 'take' problem
-- Option 1:
-- iter :: IterX vec (GenT vec m) ()
--
-- then void $ runIterX gen iter ==> Producer m vec
--
-- Option 2:
-- same as the paper, instead of using iteratee constructs, rewrite the
-- header parsing stuff in terms of streamG.
--
-- so we'd do
--
-- streamG audioFormatter af0 . streamG chunkSerializer cr0 . streamG riffWaveReader rwr0
--
-- Don't know which would have better performance, maybe option 2?
-- Option 1 looks cleaner, but I think people will have trouble with it maybe?
--
   
-- | Return the first format chunk and the consumed count to get to the chunk
-- header
nextFormatChunk :: Monad m => IterX (V.Vector Word8) m (Int,AudioFormat)
nextFormatChunk = do
    t1 <- waveChunkType
    case t1 of
        WaveFmt -> IterX.drop 4 >> (0,) <$> sWaveFormat
        _ -> loop 4
  where
    loop !acc = do
            cnt <- fromIntegral <$> getWord32le
            IterX.drop cnt
            t <- waveChunkType
            let acc' = (acc+8+cnt) -- 8 for size+next chunk header
            case t of
                WaveFmt -> IterX.drop 4 >> (acc'-4,) <$> sWaveFormat
                _ -> loop acc'

-- | An alternative way of reading a Wave file, reading over the file in one
-- direct pass instead of constructing a dictionary.
directWaveReader
  :: (ExIO m, Functor m)
  => IterX (V.Vector Word8) m
       (AudioFormat, Transducer m m (V.Vector Word8) (V.Vector Double))
directWaveReader = do
  error "crap"
{-
  isRiff <- readRiff
  when (not isRiff) $ fail "Bad RIFF header: "
  tot_size <- endianRead4 LSB
  isWave <- readRiffWave
  when (not isWave) $ fail "Bad WAVE header: "
  loop1
 where
  loop1 = do
    typ <- I.joinI $ I.take 4 stream2stream
    case waveChunk typ of
      Nothing -> throwErr $ iterStrExc "No format or data chunks found"
      Just WaveFmt -> do
          IterX.drop 4
          audioFormat <- maybeIter sWaveFormat "Invalid format: not enough bytes"
          return (audioFormat, loop audioFormat)
      Just WaveData -> throwErr $ iterStrExc "No format found before DATA chunk"
      Just (WaveOther _) -> do
          count <- getWord32le
          IterX.drop $ fromIntegral count
          loop1
  loop audioFormat iter = do
    typ <- I.joinI $ I.take 4 stream2stream
    case waveChunk typ of
      Nothing -> return iter
      Just WaveFmt -> do
          IterX.drop 4
          audioFormat' <- maybeIter sWaveFormat "Invalid format: not enough bytes"
          loop audioFormat' iter
      Just WaveData -> do
          count <- getWord32le
          nextIter <- I.takeUpTo (fromIntegral count) ><> convStream (convFunc audioFormat) $ iter
          if I.isIterFinished nextIter then return nextIter else loop audioFormat nextIter
      Just (WaveOther _) -> do
          count <- getWord32le
          IterX.drop $ fromIntegral count
          loop audioFormat iter
-}

-- |Read the RIFF header of a file.  Returns True if the file is a valid RIFF.
readRiff :: (Monad m) => IterX (V.Vector Word8) m (V.Vector Word8)
readRiff = match $ V.fromList $ fmap (fromIntegral . ord) "RIFF"

-- | Read the WAVE part of the RIFF header.  Returns True if the file is
--   a valid WAVE, otherwise False.
readRiffWave :: (Monad m) => IterX (V.Vector Word8) m (V.Vector Word8)
readRiffWave = match $ V.fromList $ fmap (fromIntegral . ord) "WAVE"

-- |An Iteratee to read a wave format chunk
sWaveFormat :: (Monad m) => IterX (V.Vector Word8) m AudioFormat
sWaveFormat = do
  f' <- getWord16le
  nc <- getWord16le
  sr <- getWord32le
  IterX.drop 6
  bd <- getWord16le
  if f' == 1
    then return  $ AudioFormat (fromIntegral nc)
                               (fromIntegral sr)
                               (fromIntegral bd)
    else fail $ "sWaveFormat: invalid format code " ++ show f'
