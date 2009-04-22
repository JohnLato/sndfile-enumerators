{-# LANGUAGE RankNTypes #-}
module Sound.Iteratee.Codecs.Wave (
  -- * Types
  -- ** Internal types
  WaveCodec (..),
  WAVEDE (..),
  WAVEDE_ENUM (..),
  -- ** WAVE CHUNK types
  WAVE_CHUNK (..),
  chunk_to_string,
  -- * Wave reading Iteratees
  -- ** Basic wave reading
  wave_reader,
  read_riff,
  wave_chunk,
  -- ** WAVE Dictionary reading/processing functions
  dict_read_format,
  dict_read_first_format,
  dict_read_last_format,
  dict_read_first_data,
  dict_read_last_data,
  dict_read_data,
  dict_process_data,
  -- ** Information on WAVE chunks
  dict_get_length_raw,
  dict_get_data_length,
  dict_sound_info,
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

import Sound.Iteratee.Codecs.WaveWriter
import Sound.Iteratee.Base
import qualified Data.Iteratee.Base as Iter
import Sound.Iteratee.Instances()
import Sound.Iteratee.Codecs.Common
import Data.Iteratee.Base
import Data.Iteratee.Binary
import qualified Data.StorableVector as SV
import qualified Data.StorableVector.Base as VB
import Control.Monad.Trans
import Control.Parallel.Strategies
import Data.Int
import Data.Word
import qualified Data.IntMap as IM

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
wave_chunk :: String -> Maybe WAVE_CHUNK
wave_chunk str
  | str == "fmt " = Just WAVE_FMT
  | str == "data" = Just WAVE_DATA
  | length str == 4 = Just $ WAVE_OTHER str
  | otherwise = Nothing

-- |Convert a WAVE_CHUNK to the representative string
chunk_to_string :: WAVE_CHUNK -> String
chunk_to_string WAVE_FMT = "fmt "
chunk_to_string WAVE_DATA = "data"
chunk_to_string (WAVE_OTHER str) = str

-- -----------------

-- |The library function to read the WAVE dictionary
wave_reader :: (MonadIO m, Functor m) => IterateeGM V Word8 m (Maybe WAVEDict)
wave_reader = do
  read_riff
  bindm (endian_read4 LSB) $ \tot_size -> do
    read_riff_wave
    chunks_m <- find_chunks $ fromIntegral tot_size
    load_dict $ join_m chunks_m

-- |Read the RIFF header of a file.
read_riff :: Monad m => IterateeGM V Word8 m ()
read_riff = do
  s <- string_read4
  case s == Just "RIFF" of
    True -> return ()
    False -> Iter.iterErr $ "Bad RIFF header: " ++ show s

-- | Read the WAVE part of the RIFF header.
read_riff_wave :: Monad m => IterateeGM V Word8 m ()
read_riff_wave = do
  s <- string_read4
  case s == Just "WAVE" of
    True -> return ()
    False -> Iter.iterErr $ "Bad RIFF/WAVE header: " ++ show s

-- | An internal function to find all the chunks.  It assumes that the
-- stream is positioned to read the first chunk.
find_chunks :: Monad m =>
               Int ->
               IterateeGM V Word8 m (Maybe [(Int, WAVE_CHUNK, Int)])
find_chunks n = find_chunks' 12 []
  where
  find_chunks' offset acc = do
    mpad <- Iter.peek
    if (offset `mod` 2 == 1) && (mpad == Just 0)
      then Iter.drop 1 >> find_chunks'2 offset acc
      else find_chunks'2 offset acc
  find_chunks'2 offset acc =
    bindm string_read4 $ \typ -> do
      count <- endian_read4 LSB
      case (wave_chunk typ, count) of
        (Nothing, _) -> (Iter.iterErr $ "Bad subchunk descriptor: " ++ show typ)
          >> return Nothing
        (_, Nothing) -> Iter.iterErr "Bad subchunk length" >> return Nothing
        (Just chk, Just count') -> let newpos = offset + 8 + count' in
          case newpos >= fromIntegral n of
            True -> return . Just $ reverse $
                (fromIntegral offset, chk, fromIntegral count') : acc
            False -> do
              Iter.seek $ fromIntegral newpos
              find_chunks' newpos $
               (fromIntegral offset, chk, fromIntegral count') : acc

load_dict :: (MonadIO m, Functor m) =>
             [(Int, WAVE_CHUNK, Int)] ->
             IterateeGM V Word8 m (Maybe WAVEDict)
load_dict = foldl read_entry (return (Just IM.empty))
  where
  read_entry dictM (offset, typ, count) =
    bindm dictM $ \dict -> do
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

read_value :: (MonadIO m, Functor m) =>
              WAVEDict ->
              Int -> -- Offset
              WAVE_CHUNK -> -- Chunk type
              Int -> -- Count
              IterateeGM V Word8 m (Maybe WAVEDE_ENUM)
read_value _dict offset _ 0 = do
  Iter.iterErr $ "Zero count in the entry of chunk at: " ++ show offset
  return Nothing

-- TODO: In order to return a partial iteratee (rather than a full value),
-- I can't use joinI because that sends EOF to the inner stream.  This may
-- regain composability of iteratees for multiple files.
read_value dict offset WAVE_DATA count = do
  fmt_m <- dict_read_last_format dict
  case fmt_m of
    Just fmt ->
      (return . Just . WEN_DUB $ \iter_dub -> do
        Iter.seek (8 + fromIntegral offset)
        let iter = convStream (conv_func fmt) iter_dub
        joinI $ joinI $ Iter.takeR count ==<< iter) `demanding` rnf fmt
    Nothing -> do
      Iter.iterErr $ "No valid format for data chunk at: " ++ show offset
      return Nothing

-- return the WaveFormat iteratee
read_value _dict offset WAVE_FMT count =
  return . Just . WEN_BYTE $ \iter -> do
    Iter.seek (8 + fromIntegral offset)
    joinI $ Iter.takeR count iter

-- for WAVE_OTHER, return Word8s and maybe the user can parse them
read_value _dict offset (WAVE_OTHER _str) count =
  return . Just . WEN_BYTE $ \iter -> do
    Iter.seek (8 + fromIntegral offset)
    joinI $ Iter.takeR count iter

-- |An Iteratee to read a wave format chunk
sWaveFormat :: Monad m => IterateeGM V Word8 m (Maybe AudioFormat)
sWaveFormat =
  bindm (endian_read2 LSB) $ \f' -> --data format, 1==PCM
   bindm (endian_read2 LSB) $ \nc ->
   bindm (endian_read4 LSB) $ \sr -> do
     Iter.drop 6
     bindm (endian_read2 LSB) $ \bd ->
       case f' == 1 of
         True -> return . Just $ AudioFormat (fromIntegral nc)
                                             (fromIntegral sr)
                                             (fromIntegral bd)
         False -> return Nothing

-- ---------------------
-- functions to assist with reading from the dictionary

-- |Read the first format chunk in the WAVE dictionary.
dict_read_first_format :: (MonadIO m, Functor m) =>
                          WAVEDict ->
                          IterateeGM V Word8 m (Maybe AudioFormat)
dict_read_first_format dict = case IM.lookup (fromEnum WAVE_FMT) dict of
  Just [] -> return Nothing
  Just ((WAVEDE _ WAVE_FMT (WEN_BYTE enum)) : _xs) -> enum ==<< sWaveFormat
  _ -> return Nothing

-- |Read the last fromat chunk from the WAVE dictionary.  This is useful
-- when parsing all chunks in the dictionary.
dict_read_last_format :: (MonadIO m, Functor m) =>
                         WAVEDict ->
                         IterateeGM V Word8 m (Maybe AudioFormat)
dict_read_last_format dict = case IM.lookup (fromEnum WAVE_FMT) dict of
  Just [] -> return Nothing
  Just xs -> let (WAVEDE _ WAVE_FMT (WEN_BYTE enum)) = last xs in
    enum ==<< sWaveFormat
  _ -> return Nothing

-- |Read the specified format chunk from the WAVE dictionary
dict_read_format :: (MonadIO m, Functor m) =>
                    Int -> --Index in the format chunk list to read
                    WAVEDict -> --Dictionary
                    IterateeGM V Word8 m (Maybe AudioFormat)
dict_read_format ix dict = case IM.lookup (fromEnum WAVE_FMT) dict of
  Just xs -> let (WAVEDE _ WAVE_FMT (WEN_BYTE enum)) = (!!) xs ix in
    enum ==<< sWaveFormat
  _ -> return Nothing

-- |Read the first data chunk in the WAVE dictionary.
dict_read_first_data :: (MonadIO m, Functor m) =>
                        WAVEDict ->
                        IterateeGM V Word8 m (Maybe [Double])
dict_read_first_data dict = case IM.lookup (fromEnum WAVE_DATA) dict of
  Just [] -> return Nothing
  Just ((WAVEDE _ WAVE_DATA (WEN_DUB enum)) : _xs) -> do
       e <- enum ==<< stream2list
       return $ Just e
  _ -> return Nothing

-- |Read the last data chunk in the WAVE dictionary.
dict_read_last_data :: (MonadIO m, Functor m) =>
                       WAVEDict ->
                       IterateeGM V Word8 m (Maybe [Double])
dict_read_last_data dict = case IM.lookup (fromEnum WAVE_DATA) dict of
  Just [] -> return Nothing
  Just xs -> let (WAVEDE _ WAVE_DATA (WEN_DUB enum)) = last xs in do
    e <- enum ==<< stream2list
    return $ Just e
  _ -> return Nothing

-- |Read the specified data chunk from the WAVE dictionary.
dict_read_data :: (MonadIO m, Functor m) =>
                  Int -> --Index in the data chunk list to read
                  WAVEDict -> --Dictionary
                  IterateeGM V Word8 m (Maybe [Double])
dict_read_data ix dict = case IM.lookup (fromEnum WAVE_DATA) dict of
  Just xs -> let (WAVEDE _ WAVE_DATA (WEN_DUB enum)) = (!!) xs ix in do
    e <- enum ==<< stream2list
    return $ Just e
  _ -> return Nothing

-- |Read the specified data chunk from the dictionary, applying the
-- data to the specified IterateeGM.
dict_process_data :: (MonadIO m, Functor m) =>
                     Int -> -- Index in the data chunk list to read
                     WAVEDict -> -- Dictionary
                     IterateeGM V Double m a ->
                     IterateeGM V Word8 m (Maybe a)
dict_process_data ix dict iter = case IM.lookup (fromEnum WAVE_DATA) dict of
  Just xs -> let (WAVEDE _ WAVE_DATA (WEN_DUB enum)) = (!!) xs ix in do
    e <- enum ==<< iter
    return $ Just e
  _ -> return Nothing

-- | Get the length of data in a dictionary chunk, in bytes.
dict_get_length_raw :: WAVE_CHUNK -> -- type of chunk to read
                       Int ->        -- index in the chunk list to read
                       WAVEDict ->   -- dictionary
                       Maybe Integer -- length of chunk in bytes
dict_get_length_raw wc ix dict = IM.lookup (fromEnum wc) dict >>= \xs ->
  let (WAVEDE off _ _) = (!!) xs ix in Just (fromIntegral off)

-- | Get the length of a data chunk, in samples.
dict_get_data_length :: AudioFormat ->
                        Int ->
                        WAVEDict ->
                        Maybe Integer
dict_get_data_length af ix dict = IM.lookup (fromEnum WAVE_DATA) dict >>= \xs ->
  let (WAVEDE off _ _) = (!!) xs ix in Just (fromIntegral off `div` bd)
  where
  bd = bitDepth af `div` 8

-- ---------------------
-- combination/utility functions

-- |Get the AudioFormat and data length from a file
dict_sound_info :: (MonadIO m, Functor m) =>
                   Maybe WAVEDict ->
                   IterateeGM V Word8 m (Maybe (AudioFormat, Integer))
dict_sound_info Nothing     = return Nothing
dict_sound_info (Just dict) = do
  fmtm <- dict_read_first_format dict
  return $ fmtm >>=
           (\fmt -> fmap (\l -> (fmt, l)) $ dict_get_data_length fmt 0 dict)

