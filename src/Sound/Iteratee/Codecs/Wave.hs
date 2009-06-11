{-# LANGUAGE RankNTypes #-}
module Sound.Iteratee.Codecs.Wave (
  -- * Types
  -- ** Internal types
  WaveCodec (..),
  WAVEDE (..),
  WAVEDE_ENUM (..),
  -- ** WAVE CHUNK types
  WAVE_CHUNK (..),
  chunkToString,
  -- * Wave reading Iteratees
  -- ** Basic wave reading
  waveReader,
  readRiff,
  waveChunk,
  -- ** WAVE Dictionary reading/processing functions
  dictReadFormat,
  dictReadFirstFormat,
  dictReadLastFormat,
  dictReadFirstData,
  dictReadLastData,
  dictReadData,
  dictProcessData,
  -- ** Information on WAVE chunks
  dictGetLengthBytes,
  dictGetLengthSamples,
  dictSoundInfo,
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
import qualified Data.Iteratee.Base as Iter
import Sound.Iteratee.Instances()
import Sound.Iteratee.Codecs.Common

import Data.Iteratee.Base
import Data.Iteratee.Binary

import qualified Data.StorableVector as SV
import qualified Data.StorableVector.Base as VB

import qualified Data.IntMap as IM
import Data.Int
import Data.Word
import Data.Char (ord)

import Control.Monad.Trans
import Control.Parallel.Strategies

-- =====================================================
-- WAVE libary code

type V = SV.Vector

-- |A WAVE directory is a list associating WAVE chunks with
-- a record WAVEDE
type WAVEDict = IM.IntMap [WAVEDE]

data WAVEDE = WAVEDE{
  wavede_count :: Int, -- ^length of chunk
  wavede_type :: WAVE_CHUNK, -- ^type of chunk
  wavede_enum :: WAVEDE_ENUM -- ^enumerator to get values of chunk
  }

instance Show WAVEDE where
  show a = "Type: " ++ (show $ wavede_type a) ++ " :: Length: " ++ (show $ wavede_count a)

data WAVEDE_ENUM =
  WEN_BYTE  (forall a m. (MonadIO m, Functor m) =>
              EnumeratorGMM V Word8 V Word8 m a)
  | WEN_DUB (forall a m. (MonadIO m, Functor m) =>
              EnumeratorGMM V Word8 V Double m a)

-- |Standard WAVE Chunks
data WAVE_CHUNK = WAVE_FMT -- ^Format
  | WAVE_DATA              -- ^Data
  | WAVE_OTHER String      -- ^Other
  deriving (Eq, Ord, Show)
instance Enum WAVE_CHUNK where
  fromEnum WAVE_FMT = 1
  fromEnum WAVE_DATA = 2
  fromEnum (WAVE_OTHER _) = 3
  toEnum 1 = WAVE_FMT
  toEnum 2 = WAVE_DATA
  toEnum 3 = WAVE_OTHER ""
  toEnum _ = error "Invalid enumeration value"

-- -----------------
-- wave chunk reading/writing functions

-- |Convert a string to WAVE_CHUNK type
waveChunk :: String -> Maybe WAVE_CHUNK
waveChunk str
  | str == "fmt " = Just WAVE_FMT
  | str == "data" = Just WAVE_DATA
  | P.length str == 4 = Just $ WAVE_OTHER str
  | otherwise = Nothing

-- |Convert a WAVE_CHUNK to the representative string
chunkToString :: WAVE_CHUNK -> String
chunkToString WAVE_FMT = "fmt "
chunkToString WAVE_DATA = "data"
chunkToString (WAVE_OTHER str) = str

-- -----------------

-- |The library function to read the WAVE dictionary
waveReader :: (MonadIO m, Functor m) => IterateeG V Word8 m (Maybe WAVEDict)
waveReader = do
  readRiff
  tot_size <- endianRead4 LSB
  readRiffWave
  chunks_m <- find_chunks $ fromIntegral tot_size
  load_dict $ join_m chunks_m

-- |Read the RIFF header of a file.
readRiff :: Monad m => IterateeG V Word8 m ()
readRiff = do
  cnt <- heads $ SV.pack $ fmap (fromIntegral . ord) "RIFF"
  case cnt of
    4 -> return ()
    _ -> throwErr . Err $ "Bad RIFF header: "

-- | Read the WAVE part of the RIFF header.
readRiffWave :: Monad m => IterateeG V Word8 m ()
readRiffWave = do
  cnt <- heads $ SV.pack $ fmap (fromIntegral . ord) "WAVE"
  case cnt of
    4 -> return ()
    _ -> throwErr . Err $ "Bad RIFF/WAVE header: "

-- | An internal function to find all the chunks.  It assumes that the
-- stream is positioned to read the first chunk.
find_chunks :: Monad m =>
               Int ->
               IterateeG V Word8 m (Maybe [(Int, WAVE_CHUNK, Int)])
find_chunks n = find_chunks' 12 []
  where
  find_chunks' offset acc = do
    mpad <- Iter.peek
    if (offset `mod` 2 == 1) && (mpad == Just 0)
      then Iter.drop 1 >> find_chunks'2 offset acc
      else find_chunks'2 offset acc
  find_chunks'2 offset acc = do
    typ <- string_read4
    count <- endianRead4 LSB
    case waveChunk typ of
      Nothing -> (throwErr . Err $ "Bad subchunk descriptor: " ++ show typ)
      Just chk -> let newpos = offset + 8 + count in
        case newpos >= fromIntegral n of
          True -> return . Just . reverse $
              (fromIntegral offset, chk, fromIntegral count) : acc
          False -> do
            Iter.seek $ fromIntegral newpos
            find_chunks' newpos $
             (fromIntegral offset, chk, fromIntegral count) : acc

load_dict :: (MonadIO m, Functor m) =>
             [(Int, WAVE_CHUNK, Int)] ->
             IterateeG V Word8 m (Maybe WAVEDict)
load_dict = P.foldl read_entry (return (Just IM.empty))
  where
  read_entry dictM (offset, typ, count) = dictM >>=
    maybe (return Nothing) (\dict -> do
      enum_m <- read_value dict offset typ count
      case (enum_m, IM.lookup (fromEnum typ) dict) of
        (Just enum, Nothing) -> --insert new entry
          return . Just $ IM.insert (fromEnum typ)
                                    [WAVEDE (fromIntegral count) typ enum] dict
        (Just enum, Just _vals) -> --existing entry
          return . Just $ IM.update
            (\ls -> Just $ ls ++ [WAVEDE (fromIntegral count) typ enum])
            (fromEnum typ) dict
        (Nothing, _) -> return (Just dict)
    )

read_value :: (MonadIO m, Functor m) =>
              WAVEDict ->
              Int -> -- Offset
              WAVE_CHUNK -> -- Chunk type
              Int -> -- Count
              IterateeG V Word8 m (Maybe WAVEDE_ENUM)
read_value _dict offset _ 0 =
  throwErr . Err $ "Zero count in the entry of chunk at: " ++ show offset

-- TODO: In order to return a partial iteratee (rather than a full value),
-- I can't use joinI because that sends EOF to the inner stream.  This may
-- regain composability of iteratees for multiple files.
read_value dict offset WAVE_DATA count = do
  fmt_m <- dictReadLastFormat dict
  case fmt_m of
    Just fmt ->
      (return . Just . WEN_DUB $ \iter_dub -> return $ do
        Iter.seek (8 + fromIntegral offset)
        let iter = convStream (conv_func fmt) iter_dub
        joinI . joinI . takeR count $ iter
      ) `demanding` rnf fmt
    Nothing -> do
      throwErr . Err $ "No valid format for data chunk at: " ++ show offset
      return Nothing

-- return the WaveFormat iteratee
read_value _dict offset WAVE_FMT count =
  return . Just . WEN_BYTE $ \iter -> return $ do
    Iter.seek (8 + fromIntegral offset)
    joinI $ Iter.takeR count iter

-- for WAVE_OTHER, return Word8s and maybe the user can parse them
read_value _dict offset (WAVE_OTHER _str) count =
  return . Just . WEN_BYTE $ \iter -> return $ do
    Iter.seek (8 + fromIntegral offset)
    joinI $ Iter.takeR count iter

-- |An Iteratee to read a wave format chunk
sWaveFormat :: Monad m => IterateeG V Word8 m (Maybe AudioFormat)
sWaveFormat = do
  f' <- endianRead2 LSB
  nc <- endianRead2 LSB
  sr <- endianRead4 LSB
  Iter.drop 6
  bd <- endianRead2 LSB
  case f' == 1 of
    True -> return . Just $ AudioFormat (fromIntegral nc)
                                        (fromIntegral sr)
                                        (fromIntegral bd)
    False -> return Nothing

-- ---------------------
-- functions to assist with reading from the dictionary

-- |Read the first format chunk in the WAVE dictionary.
dictReadFirstFormat :: (MonadIO m, Functor m) =>
                       WAVEDict ->
                       IterateeG V Word8 m (Maybe AudioFormat)
dictReadFirstFormat dict = case IM.lookup (fromEnum WAVE_FMT) dict of
  Just [] -> return Nothing
  Just ((WAVEDE _ WAVE_FMT (WEN_BYTE enum)) : _xs) -> joinIM $ enum sWaveFormat
  _ -> return Nothing

-- |Read the last fromat chunk from the WAVE dictionary.  This is useful
-- when parsing all chunks in the dictionary.
dictReadLastFormat :: (MonadIO m, Functor m) =>
                      WAVEDict ->
                      IterateeG V Word8 m (Maybe AudioFormat)
dictReadLastFormat dict = case IM.lookup (fromEnum WAVE_FMT) dict of
  Just [] -> return Nothing
  Just xs -> let (WAVEDE _ WAVE_FMT (WEN_BYTE enum)) = last xs in
    joinIM $ enum sWaveFormat
  _ -> return Nothing

-- |Read the specified format chunk from the WAVE dictionary
dictReadFormat :: (MonadIO m, Functor m) =>
                  Int -> --Index in the format chunk list to read
                  WAVEDict -> --Dictionary
                  IterateeG V Word8 m (Maybe AudioFormat)
dictReadFormat ix dict = case IM.lookup (fromEnum WAVE_FMT) dict of
  Just xs -> let (WAVEDE _ WAVE_FMT (WEN_BYTE enum)) = (!!) xs ix in
    joinIM $ enum sWaveFormat
  _ -> return Nothing

-- |Read the first data chunk in the WAVE dictionary.
dictReadFirstData :: (MonadIO m, Functor m) =>
                     WAVEDict ->
                     IterateeG V Word8 m (Maybe [Double])
dictReadFirstData dict = case IM.lookup (fromEnum WAVE_DATA) dict of
  Just [] -> return Nothing
  Just ((WAVEDE _ WAVE_DATA (WEN_DUB enum)) : _xs) ->
       fmap Just (joinIM $ enum stream2list)
  _ -> return Nothing

-- |Read the last data chunk in the WAVE dictionary.
dictReadLastData :: (MonadIO m, Functor m) =>
                    WAVEDict ->
                    IterateeG V Word8 m (Maybe [Double])
dictReadLastData dict = case IM.lookup (fromEnum WAVE_DATA) dict of
  Just [] -> return Nothing
  Just xs -> let (WAVEDE _ WAVE_DATA (WEN_DUB enum)) = last xs in do
    fmap Just (joinIM $ enum stream2list)
  _ -> return Nothing

-- |Read the specified data chunk from the WAVE dictionary.
dictReadData :: (MonadIO m, Functor m) =>
                Int -> --Index in the data chunk list to read
                WAVEDict -> --Dictionary
                IterateeG V Word8 m (Maybe [Double])
dictReadData ix dict = case IM.lookup (fromEnum WAVE_DATA) dict of
  Just xs -> let (WAVEDE _ WAVE_DATA (WEN_DUB enum)) = (!!) xs ix in do
    fmap Just (joinIM $ enum stream2list)
  _ -> return Nothing

-- |Read the specified data chunk from the dictionary, applying the
-- data to the specified IterateeG.
dictProcessData :: (MonadIO m, Functor m) =>
                   Int -> -- Index in the data chunk list to read
                   WAVEDict -> -- Dictionary
                   IterateeG V Double m a ->
                   IterateeG V Word8 m (Maybe a)
dictProcessData ix dict iter = case IM.lookup (fromEnum WAVE_DATA) dict of
  Just xs -> let (WAVEDE _ WAVE_DATA (WEN_DUB enum)) = (!!) xs ix in do
    fmap Just (joinIM $ enum iter)
  _ -> return Nothing

-- | Get the length of data in a dictionary chunk, in bytes.
dictGetLengthBytes :: WAVE_CHUNK -> -- type of chunk to read
                      Int ->        -- index in the chunk list to read
                      WAVEDict ->   -- dictionary
                      Maybe Integer -- length of chunk in bytes
dictGetLengthBytes wc ix dict = IM.lookup (fromEnum wc) dict >>= \xs ->
  let (WAVEDE off _ _) = (!!) xs ix in Just (fromIntegral off)

-- | Get the length of a data chunk, in samples.
dictGetLengthSamples :: AudioFormat ->
                        Int ->
                        WAVEDict ->
                        Maybe Integer
dictGetLengthSamples af ix dict = IM.lookup (fromEnum WAVE_DATA) dict >>= \xs ->
  let (WAVEDE off _ _) = (!!) xs ix in Just (fromIntegral off `div` bd)
  where
  bd = bitDepth af `div` 8

-- ---------------------
-- combination/utility functions

-- |Get the AudioFormat and data length from a file
dictSoundInfo :: (MonadIO m, Functor m) =>
                 WAVEDict ->
                 IterateeG V Word8 m (Maybe (AudioFormat, Integer))
dictSoundInfo dict = do
  fmtm <- dictReadFirstFormat dict
  return $ fmtm >>=
           (\fmt -> fmap (\l -> (fmt, l)) $ dictGetLengthSamples fmt 0 dict)

