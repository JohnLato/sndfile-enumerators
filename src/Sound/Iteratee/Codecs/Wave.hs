{-# LANGUAGE RankNTypes, NoMonomorphismRestriction #-}
module Sound.Iteratee.Codecs.Wave (
  -- * Types
  -- ** Internal types
  WaveCodec (..),
  WAVEDE (..),
  WAVEDEENUM (..),
  -- ** WAVE CHUNK types
  WAVECHUNK (..),
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
  wavedeType :: WAVECHUNK, -- ^type of chunk
  wavedeEnum :: WAVEDEENUM -- ^enumerator to get values of chunk
  }

instance Show WAVEDE where
  show a = "Type: " ++ show (wavedeType a) ++ " :: Length: " ++
            show (wavedeCount a)

type MEnumeratorM sfrom sto m a = MIteratee sto m a -> MIteratee sfrom m a
type MEnumeratorM2 sfrom sto m a = MIteratee sto m a
                                   -> MIteratee sfrom m (MIteratee sto m a)

data WAVEDEENUM =
  WENBYTE  (forall a m r. (MonadCatchIO m, Functor m) =>
              MEnumeratorM (IOB r Word8) (IOB r Word8) m a)
  | WENDUB (forall a m r. (MonadCatchIO m, Functor m) =>
              MEnumeratorM2 (IOB r Word8) (IOB r Double) m a)

-- |Standard WAVE Chunks
data WAVECHUNK = WAVEFMT -- ^Format
  | WAVEDATA              -- ^Data
  | WAVEOTHER String      -- ^Other
  deriving (Eq, Ord, Show)
instance Enum WAVECHUNK where
  fromEnum WAVEFMT = 1
  fromEnum WAVEDATA = 2
  fromEnum (WAVEOTHER _) = 3
  toEnum 1 = WAVEFMT
  toEnum 2 = WAVEDATA
  toEnum 3 = WAVEOTHER ""
  toEnum _ = error "Invalid enumeration value"

-- -----------------
-- wave chunk reading/writing functions

-- |Convert a string to WAVECHUNK type
waveChunk :: String -> Maybe WAVECHUNK
waveChunk str
  | str == "fmt " = Just WAVEFMT
  | str == "data" = Just WAVEDATA
  | P.length str == 4 = Just $ WAVEOTHER str
  | otherwise = Nothing

-- |Convert a WAVECHUNK to the representative string
chunkToString :: WAVECHUNK -> String
chunkToString WAVEFMT = "fmt "
chunkToString WAVEDATA = "data"
chunkToString (WAVEOTHER str) = str

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
  -> MIteratee (IOB r Word8) m (Maybe [(Int, WAVECHUNK, Int)])
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
        if newpos >= fromIntegral n
          then return . Just . reverse $
               (fromIntegral offset, chk, fromIntegral count) : acc
          else do
            MIteratee . Itr.seek $ fromIntegral newpos
            findChunks' newpos $
             (fromIntegral offset, chk, fromIntegral count) : acc

loadDict ::
 (MonadCatchIO m, Functor m) =>
  [(Int, WAVECHUNK, Int)]
  -> MIteratee (IOB r Word8) m (Maybe WAVEDict)
loadDict = P.foldl read_entry (return (Just IM.empty))
  where
  read_entry dictM (offset, typ, count) = dictM >>=
    maybe (return Nothing) (\dict -> do
      enum_m <- readValue dict offset typ count
      case (enum_m, IM.lookup (fromEnum typ) dict) of
        (Just enum, Nothing) -> --last entry
          return . Just $ IM.insert (fromEnum typ)
                                    [WAVEDE (fromIntegral count) typ enum] dict
        (Just enum, Just _vals) -> --more entries to come
          return . Just $ IM.update
            (\ls -> Just $ ls ++ [WAVEDE (fromIntegral count) typ enum])
            (fromEnum typ) dict
        (Nothing, _) -> return (Just dict)
    )

readValue ::
 (MonadCatchIO m, Functor m) =>
  WAVEDict
  -> Int -- ^ Offset
  -> WAVECHUNK -- ^ Chunk type
  -> Int -- ^ Count
  -> MIteratee (IOB r Word8) m (Maybe WAVEDEENUM)
readValue _dict offset _ 0 = MIteratee . throwErr . iterStrExc $
  "Zero count in the entry of chunk at: " ++ show offset

readValue dict offset WAVEDATA count = do
  fmt_m <- dictReadLastFormat dict
  case fmt_m of
    Just fmt ->
      fmt `seq` (return . Just . WENDUB $ \iter_dub -> do
        MIteratee $ Itr.seek (8 + fromIntegral offset)
        offp <- liftIO $ new 0 >>= newForeignPtr_
        bufp <- liftIO $ mallocArray defaultChunkLength >>= newForeignPtr_
        let iter = convStream (convFunc fmt offp bufp) iter_dub
        joinIob . takeR count $ iter
      )
    Nothing ->
      MIteratee . throwErr . iterStrExc $
        "No valid format for data chunk at: " ++ show offset

-- return the WaveFormat iteratee
readValue _dict offset WAVEFMT count =
  return . Just . WENBYTE $ \iter -> do
    MIteratee $ Itr.seek (8 + fromIntegral offset)
    joinIob $ MI.takeR count iter

-- for WAVEOTHER, return Word8s and maybe the user can parse them
readValue _dict offset (WAVEOTHER _str) count =
  return . Just . WENBYTE $ \iter -> do
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
  if f' == 1
    then return . Just $ AudioFormat (fromIntegral nc)
                                     (fromIntegral sr)
                                     (fromIntegral bd)
    else return Nothing

-- ---------------------
-- functions to assist with reading from the dictionary

-- |Read the first format chunk in the WAVE dictionary.
dictReadFirstFormat ::
 (MonadCatchIO m, Functor m) =>
  WAVEDict
  -> MIteratee (IOB r Word8) m (Maybe AudioFormat)
dictReadFirstFormat dict = case IM.lookup (fromEnum WAVEFMT) dict of
  Just [] -> return Nothing
  Just (WAVEDE _ WAVEFMT (WENBYTE enum) : _xs) -> enum sWaveFormat
  _ -> return Nothing

-- |Read the last fromat chunk from the WAVE dictionary.  This is useful
-- when parsing all chunks in the dictionary.
dictReadLastFormat ::
 (MonadCatchIO m, Functor m) =>
  WAVEDict
  -> MIteratee (IOB r Word8) m (Maybe AudioFormat)
dictReadLastFormat dict = case IM.lookup (fromEnum WAVEFMT) dict of
  Just [] -> return Nothing
  Just xs -> let (WAVEDE _ WAVEFMT (WENBYTE enum)) = last xs
             in enum sWaveFormat
  _ -> return Nothing

-- |Read the specified format chunk from the WAVE dictionary
dictReadFormat ::
 (MonadCatchIO m, Functor m) =>
  Int -- ^ Index in the format chunk list to read
  -> WAVEDict -- ^ Dictionary
  -> MIteratee (IOB r Word8) m (Maybe AudioFormat)
dictReadFormat ix dict = case IM.lookup (fromEnum WAVEFMT) dict of
  Just xs -> let (WAVEDE _ WAVEFMT (WENBYTE enum)) = xs !! ix
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
dictProcessData ix dict iter = case IM.lookup (fromEnum WAVEDATA) dict of
  Just xs -> let (WAVEDE _ WAVEDATA (WENDUB enum)) = (!!) xs ix
             in (enum iter)
  _ -> error "didn't find requested enumerator in WAVEDict for dictProcessData"

dictProcessData_ ::
 (MonadCatchIO m, Functor m) =>
  Int -- ^ Index in the data chunk list to read
  -> WAVEDict -- ^ Dictionary
  -> MIteratee (IOB r Double) m a
  -> MIteratee (IOB r Word8) m (Maybe a)
dictProcessData_ ix dict iter = case IM.lookup (fromEnum WAVEDATA) dict of
  Just xs -> let (WAVEDE _ WAVEDATA (WENDUB enum)) = (!!) xs ix
             in fmap Just . joinIob . enum $ iter
  _ -> return Nothing

-- | Get the length of data in a dictionary chunk, in bytes.
dictGetLengthBytes :: WAVECHUNK -> -- type of chunk to read
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
dictGetLengthSamples af ix dict = IM.lookup (fromEnum WAVEDATA) dict >>= \xs ->
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

