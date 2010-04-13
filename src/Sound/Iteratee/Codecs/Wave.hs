{-# LANGUAGE RankNTypes, NoMonomorphismRestriction #-}
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
  dictProcessData,
  dictProcessData_,
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
import Sound.Iteratee.Codecs.Common

import qualified Data.MutableIter.IOBuffer as IB
import Data.MutableIter as MI
import Data.MutableIter.Binary

import qualified Data.Iteratee as Itr
import Data.Iteratee (throwErr, iterStrExc)

import qualified Data.IntMap as IM
import Data.Word
import Data.Char (ord)

import Control.Monad.CatchIO
import Control.Monad.Trans

import Foreign.Marshal.Utils
import Foreign.Marshal.Array
import Foreign.ForeignPtr

-- =====================================================
-- WAVE libary code

type IOB = IB.IOBuffer

-- |A WAVE directory is a list associating WAVE chunks with
-- a record WAVEDE
type WAVEDict = IM.IntMap [WAVEDE]

data WAVEDE = WAVEDE{
  wavedeCount :: Int, -- ^length of chunk
  wavedeType :: WAVE_CHUNK, -- ^type of chunk
  wavedeEnum :: WAVEDE_ENUM -- ^enumerator to get values of chunk
  }

instance Show WAVEDE where
  show a = "Type: " ++ show (wavedeType a) ++ " :: Length: " ++
            show (wavedeCount a)

type MEnumeratorM sfrom sto m a = MIteratee sto m a -> (MIteratee sfrom m a)
type MEnumeratorM2 sfrom sto m a = MIteratee sto m a -> (MIteratee sfrom m (MIteratee sto m a))

data WAVEDE_ENUM =
  WEN_BYTE  (forall a m r. (MonadCatchIO m, Functor m) =>
              MEnumeratorM (IOB r Word8) (IOB r Word8) m a)
  | WEN_DUB (forall a m r. (MonadCatchIO m, Functor m) =>
              MEnumeratorM2 (IOB r Word8) (IOB r Double) m a)

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
waveReader ::
 (MonadCatchIO m, Functor m) =>
   MIteratee (IOB r Word8) m (Maybe WAVEDict)
waveReader = do
  readRiff
  tot_size <- endianRead4 LSB
  readRiffWave
  chunks_m <- findChunks $ fromIntegral tot_size
  loadDict $ joinMaybe chunks_m

-- |Read the RIFF header of a file.
readRiff :: MonadCatchIO m => MIteratee (IOB r Word8) m ()
readRiff = do
  cnt <- heads $ fmap (fromIntegral . ord) "RIFF"
  case cnt of
    4 -> return ()
    _ -> MIteratee . throwErr . iterStrExc $ "Bad RIFF header: "

-- | Read the WAVE part of the RIFF header.
readRiffWave :: MonadCatchIO m => MIteratee (IOB r Word8) m ()
readRiffWave = do
  cnt <- heads $ fmap (fromIntegral . ord) "WAVE"
  case cnt of
    4 -> return ()
    _ -> MIteratee . throwErr . iterStrExc $ "Bad RIFF/WAVE header: "

-- | An internal function to find all the chunks.  It assumes that the
-- stream is positioned to read the first chunk.
findChunks ::
 MonadCatchIO m =>
  Int
  -> MIteratee (IOB r Word8) m (Maybe [(Int, WAVE_CHUNK, Int)])
findChunks n = findChunks' 12 []
  where
  findChunks' offset acc = do
    mpad <- MI.peek
    if (offset `rem` 2 == 1) && (mpad == Just 0)
      then MI.drop 1 >> findChunks'2 offset acc
      else findChunks'2 offset acc
  findChunks'2 offset acc = do
    typ <- stringRead4
    count <- endianRead4 LSB
    case waveChunk typ of
      Nothing -> (MIteratee . throwErr . iterStrExc $
        "Bad subchunk descriptor: " ++ show typ)
      Just chk -> let newpos = offset + 8 + count in
        case newpos >= fromIntegral n of
          True -> return . Just . reverse $
              (fromIntegral offset, chk, fromIntegral count) : acc
          False -> do
            MIteratee . Itr.seek $ fromIntegral newpos
            findChunks' newpos $
             (fromIntegral offset, chk, fromIntegral count) : acc

loadDict ::
 (MonadCatchIO m, Functor m) =>
  [(Int, WAVE_CHUNK, Int)]
  -> MIteratee (IOB r Word8) m (Maybe WAVEDict)
loadDict = P.foldl read_entry (return (Just IM.empty))
  where
  read_entry dictM (offset, typ, count) = dictM >>=
    maybe (return Nothing) (\dict -> do
      enum_m <- readValue dict offset typ count
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

readValue ::
 (MonadCatchIO m, Functor m) =>
  WAVEDict
  -> Int -- ^ Offset
  -> WAVE_CHUNK -- ^ Chunk type
  -> Int -- ^ Count
  -> MIteratee (IOB r Word8) m (Maybe WAVEDE_ENUM)
readValue _dict offset _ 0 = MIteratee . throwErr . iterStrExc $
  "Zero count in the entry of chunk at: " ++ show offset

-- TODO: In order to return a partial iteratee (rather than a full value),
-- I can't use joinI because that sends EOF to the inner stream.
readValue dict offset WAVE_DATA count = do
  fmt_m <- dictReadLastFormat dict
  case fmt_m of
    Just fmt ->
      fmt `seq` (return . Just . WEN_DUB $ \iter_dub -> do
        MIteratee $ Itr.seek (8 + fromIntegral offset)
        offp <- liftIO $ new 0 >>= newForeignPtr_
        bufp <- liftIO $ mallocArray defaultChunkLength >>= newForeignPtr_
        let iter = convStream (convFunc fmt offp bufp) iter_dub
        joinIob . takeR count $ iter
      )
    Nothing -> do
      MIteratee . throwErr . iterStrExc $
        "No valid format for data chunk at: " ++ show offset

-- return the WaveFormat iteratee
readValue _dict offset WAVE_FMT count =
  return . Just . WEN_BYTE $ \iter -> do
    MIteratee $ Itr.seek (8 + fromIntegral offset)
    joinIob $ MI.takeR count iter

-- for WAVE_OTHER, return Word8s and maybe the user can parse them
readValue _dict offset (WAVE_OTHER _str) count =
  return . Just . WEN_BYTE $ \iter -> do
    MIteratee $ Itr.seek (8 + fromIntegral offset)
    joinIob $ MI.takeR count iter

-- |An Iteratee to read a wave format chunk
sWaveFormat :: MonadCatchIO m =>
  MIteratee (IOB r Word8) m (Maybe AudioFormat)
sWaveFormat = do
  f' <- endianRead2 LSB
  nc <- endianRead2 LSB
  sr <- endianRead4 LSB
  MI.drop 6
  bd <- endianRead2 LSB
  case f' == 1 of
    True -> return . Just $ AudioFormat (fromIntegral nc)
                                        (fromIntegral sr)
                                        (fromIntegral bd)
    False -> return Nothing

-- ---------------------
-- functions to assist with reading from the dictionary

-- |Read the first format chunk in the WAVE dictionary.
dictReadFirstFormat ::
 (MonadCatchIO m, Functor m) =>
  WAVEDict
  -> MIteratee (IOB r Word8) m (Maybe AudioFormat)
dictReadFirstFormat dict = case IM.lookup (fromEnum WAVE_FMT) dict of
  Just [] -> return Nothing
  Just ((WAVEDE _ WAVE_FMT (WEN_BYTE enum)) : _xs) -> enum sWaveFormat
  _ -> return Nothing

-- |Read the last fromat chunk from the WAVE dictionary.  This is useful
-- when parsing all chunks in the dictionary.
dictReadLastFormat ::
 (MonadCatchIO m, Functor m) =>
  WAVEDict
  -> MIteratee (IOB r Word8) m (Maybe AudioFormat)
dictReadLastFormat dict = case IM.lookup (fromEnum WAVE_FMT) dict of
  Just [] -> return Nothing
  Just xs -> let (WAVEDE _ WAVE_FMT (WEN_BYTE enum)) = last xs
             in enum sWaveFormat
  _ -> return Nothing

-- |Read the specified format chunk from the WAVE dictionary
dictReadFormat ::
 (MonadCatchIO m, Functor m) =>
  Int -- ^ Index in the format chunk list to read
  -> WAVEDict -- ^ Dictionary
  -> MIteratee (IOB r Word8) m (Maybe AudioFormat)
dictReadFormat ix dict = case IM.lookup (fromEnum WAVE_FMT) dict of
  Just xs -> let (WAVEDE _ WAVE_FMT (WEN_BYTE enum)) = xs !! ix
             in enum sWaveFormat
  _ -> return Nothing

-- |Read the specified data chunk from the dictionary, applying the
-- data to the specified MIteratee.
dictProcessData ::
 (MonadCatchIO m, Functor m) =>
  Int -- ^ Index in the data chunk list to read
  -> WAVEDict -- ^ Dictionary
  -> MIteratee (IOB r Double) m a
  -> MIteratee (IOB r Word8) m (MIteratee (IOB r Double) m a)
dictProcessData ix dict iter = case IM.lookup (fromEnum WAVE_DATA) dict of
  Just xs -> let (WAVEDE _ WAVE_DATA (WEN_DUB enum)) = (!!) xs ix
             in (enum iter)
  _ -> error "didn't find requested enumerator in WAVEDict for dictProcessData"

dictProcessData_ ::
 (MonadCatchIO m, Functor m) =>
  Int -- ^ Index in the data chunk list to read
  -> WAVEDict -- ^ Dictionary
  -> MIteratee (IOB r Double) m a
  -> MIteratee (IOB r Word8) m (Maybe a)
dictProcessData_ ix dict iter = case IM.lookup (fromEnum WAVE_DATA) dict of
  Just xs -> let (WAVEDE _ WAVE_DATA (WEN_DUB enum)) = (!!) xs ix
             in (fmap Just) . joinIob . enum $ iter
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
dictSoundInfo ::
 (MonadCatchIO m, Functor m) =>
  WAVEDict
  -> MIteratee (IOB r Word8) m
      (Maybe (AudioFormat, Integer))
dictSoundInfo dict = do
  fmtm <- dictReadFirstFormat dict
  return $ fmtm >>=
           (\fmt -> fmap (\l -> (fmt, l)) $ dictGetLengthSamples fmt 0 dict)

