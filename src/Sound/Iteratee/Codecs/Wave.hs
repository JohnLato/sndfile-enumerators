{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -Wall #-}
module Sound.Iteratee.Codecs.Wave (
  -- * Types
  -- ** Internal types
  WaveCodec (..),
  WaveDict,
  WaveChunk (..),
  -- ** WAVE CHUNK types
  WaveChunkType (..),
  chunkToString,
  -- * Wave reading Iteratees
  -- ** Basic wave reading
  waveReadDict,
  directWaveReader,
  readRiff,
  waveChunk,
  -- ** Information on WAVE chunks
  dictSoundInfo,
  simpleDictSoundInfo,
  dictFormats,
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

import           Data.Iteratee as I

import qualified Data.List   as L
import           Data.Maybe
import           Data.Word
import           Data.Char (chr, ord)

import           Control.Arrow (first)
import           Control.Monad
import           Control.Monad.Trans.Control
import           Control.Monad.IO.Class

-- =====================================================
-- WAVE libary code

-- |A wave directory is a list of 'WaveChunk's
type WaveDict = [WaveChunk]

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
waveChunk vec
  | vec == V.fromList [102,109,116,32] = Just WaveFmt   -- "fmt "
  | vec == V.fromList [100,97,116,97]  = Just WaveData  -- "data"
  | V.length vec == 4 = Just . WaveOther $ map (chr . fromIntegral) vlist
  | otherwise = Nothing
  where
    vlist = V.toList vec

-- |Convert a WaveChunkType to the representative string
chunkToString :: WaveChunkType -> String
chunkToString WaveFmt = "fmt "
chunkToString WaveData = "data"
chunkToString (WaveOther str) = str

-- -----------------

-- |The library function to read the WAVE dictionary
waveReadDict
  :: (MonadIO m, MonadBaseControl IO m, Functor m)
  => Iteratee (V.Vector Word8) m WaveDict
waveReadDict = do
  isRiff <- readRiff
  when (not isRiff) $ throwErr . iterStrExc $ "Bad RIFF header: "
  tot_size <- endianRead4 LSB
  isWave <- readRiffWave
  when (not isWave) $ throwErr . iterStrExc $ "Bad WAVE header: "
  chunks <- findChunks $ fromIntegral tot_size
  let mkChunk (waveOffset,waveCount,Left WaveData)    = WaveDataChunk{..}
      mkChunk (waveOffset,waveCount,Right waveFormat) = WaveFmtChunk{..}
      mkChunk (waveOffset,waveCount,Left (WaveOther waveOtherType)) =
        WaveOtherChunk{..}
  return $ foldr (\tup -> (mkChunk tup:)) [] chunks

-- | Create a list of (AudioFormat,DataChunk) pairs from a WaveDict
tagDict :: WaveDict -> [(AudioFormat, WaveChunk)]
tagDict = finish . P.foldl f (Nothing, id)
  where
    finish = ($ []) . snd
    f (_,acc) (WaveFmtChunk{..}) = (Just waveFormat, acc)
    f (fm@(Just fmt),acc) (chk@WaveDataChunk{..}) = (fm,acc . ((fmt,chk):))
    f acc _ = acc

-- | An alternative way of reading a Wave file, reading over the file in one
-- direct pass instead of constructing a dictionary.
directWaveReader
  :: (MonadIO m, MonadBaseControl IO m, Functor m)
  => Iteratee (V.Vector Word8) m
       (AudioFormat, Enumeratee (V.Vector Word8) (V.Vector Double) m a)
directWaveReader = do
  isRiff <- readRiff
  when (not isRiff) $ throwErr . iterStrExc $ "Bad RIFF header: "
  I.drop 4  -- total size
  isWave <- readRiffWave
  when (not isWave) $ throwErr . iterStrExc $ "Bad WAVE header: "
  loop1
 where
  loop1 = do
    typ <- I.joinI $ I.take 4 stream2stream
    case waveChunk typ of
      Nothing -> throwErr $ iterStrExc "No format or data chunks found"
      Just WaveFmt -> do
          I.drop 4
          audioFormat <- maybeIter sWaveFormat "Invalid format: not enough bytes"
          return (audioFormat, loop audioFormat)
      Just WaveData -> throwErr $ iterStrExc "No format found before DATA chunk"
      Just (WaveOther _) -> do
          count <- endianRead4 LSB
          I.drop $ fromIntegral count
          loop1
  loop audioFormat iter = do
    typ <- I.joinI $ I.take 4 stream2stream
    case waveChunk typ of
      Nothing -> return iter
      Just WaveFmt -> do
          I.drop 4
          audioFormat' <- maybeIter sWaveFormat "Invalid format: not enough bytes"
          loop audioFormat' iter
      Just WaveData -> do
          count <- endianRead4 LSB
          nextIter <- I.takeUpTo (fromIntegral count) ><> convStream (convFunc audioFormat) $ iter
          if I.isIterFinished nextIter then return nextIter else loop audioFormat nextIter
      Just (WaveOther _) -> do
          count <- endianRead4 LSB
          I.drop $ fromIntegral count
          loop audioFormat iter

-- |Read the RIFF header of a file.  Returns True if the file is a valid RIFF.
readRiff :: (Monad m) => Iteratee (V.Vector Word8) m Bool
readRiff = do
  cnt <- heads . V.fromList $ fmap (fromIntegral . ord) "RIFF"
  case cnt of
    4 -> return True
    _ -> return False

-- | Read the WAVE part of the RIFF header.  Returns True if the file is
--   a valid WAVE, otherwise False.
readRiffWave :: (Monad m) => Iteratee (V.Vector Word8) m Bool
readRiffWave = do
  cnt <- heads . V.fromList $ fmap (fromIntegral . ord) "WAVE"
  case cnt of
    4 -> return True
    _ -> return False

type ReadChunk = (Int, Int, Either WaveChunkType AudioFormat)

-- | An internal function to find all the chunks.  It assumes that the
-- stream is positioned to read the first chunk, 12 bytes into the stream.
findChunks
  :: (MonadIO m, MonadBaseControl IO m)
  => Int
  -> Iteratee (V.Vector Word8) m ([ReadChunk])
findChunks n = findChunks' 12 []
  where
  findChunks' offset acc
    | offset >= fromIntegral n = return $ reverse acc
    | otherwise = do
        mpad <- I.peek
        if (offset `rem` 2 == 1) && (mpad == Just 0)
          then I.drop 1 >> findChunks'2 offset acc
          else findChunks'2 offset acc
  findChunks'2 offset acc = do
    typ <- I.joinI $ I.take 4 stream2stream
    count <- endianRead4 LSB
    case waveChunk typ of
      Nothing -> (throwErr . iterStrExc $
        "Bad subchunk descriptor: " ++ show typ)
      Just WaveFmt -> do
        audioFormat <- maybeIter sWaveFormat "invalid FMT chunk"
        findChunks' (offset + 8 + count) $
          (fromIntegral offset, fromIntegral count, Right audioFormat) : acc
      Just chk -> do
        let newpos = offset + 8 + count
        I.seek $ fromIntegral newpos
        findChunks' newpos $
          (fromIntegral offset, fromIntegral count, Left chk) : acc

-- |An Iteratee to read a wave format chunk
sWaveFormat :: (MonadIO m, MonadBaseControl IO m) =>
  Iteratee (V.Vector Word8) m (Maybe AudioFormat)
sWaveFormat = do
  f' <- endianRead2 LSB
  nc <- endianRead2 LSB
  sr <- endianRead4 LSB
  I.drop 6
  bd <- endianRead2 LSB
  if f' == 1
    then return . Just $ AudioFormat (fromIntegral nc)
                                     (fromIntegral sr)
                                     (fromIntegral bd)
    else return Nothing

maybeIter :: Monad m => Iteratee s m (Maybe a) -> String -> Iteratee s m a
maybeIter i err = i >>= maybe (throwErr $ iterStrExc err) return

-- ---------------------
-- combination/utility functions

dictFormats :: WaveDict -> [AudioFormat]
dictFormats = mapMaybe f
  where
    f WaveFmtChunk{..} = Just waveFormat
    f _                = Nothing

-- |Get the AudioFormat and data length (in samples) from a file
-- 
dictSoundInfo :: WaveDict -> ([AudioFormat], Integer)
dictSoundInfo = first reverse . L.foldl' f ([],0) . tagDict
  where
    getLengthSamples AudioFormat{..} ck =
      fromIntegral (waveCount ck) `mod` (fromIntegral bitDepth `mod` 8)
    f (afs,!len) (af,ck) = (af:afs, len + getLengthSamples af ck)

simpleDictSoundInfo :: WaveDict -> Maybe (AudioFormat, Integer)
simpleDictSoundInfo dict = case dictSoundInfo dict of
  ([af],len) -> Just (af,len)
  _          -> Nothing
