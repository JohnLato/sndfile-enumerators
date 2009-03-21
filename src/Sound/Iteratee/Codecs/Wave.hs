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
import Data.Iteratee.Base
import Data.Iteratee.Binary
import qualified Data.StorableVector as Vec
import qualified Data.StorableVector.Base as VB
import qualified Foreign.Ptr as FP
import qualified Foreign.ForeignPtr as FFP
import Foreign.Storable
import qualified Foreign.Marshal.Utils as FMU
import Control.Monad.Trans
import Data.Char (chr)
import Data.Int
import Data.Word
import Data.Bits (shiftL)
import qualified Data.IntMap as IM
import System.IO

-- =====================================================
-- WAVE libary code

-- useful type synonyms

type V    = Vec.Vector

-- determine host endian-ness
be :: IO Bool
be = fmap (==1) $ FMU.with (1 :: Word16) (\p -> peekByteOff p 1 :: IO Word8)

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

-- convenience function to read a 4-byte ASCII string
string_read4 :: Monad m => IterateeGM V Word8 m (Maybe String)
string_read4 =
  bindm Iter.head $ \s1 ->
   bindm Iter.head $ \s2 ->
   bindm Iter.head $ \s3 ->
   bindm Iter.head $ \s4 ->
   return . Just $ map (chr . fromIntegral) [s1, s2, s3, s4]

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
      then find_chunks'2 offset acc
      else Iter.drop 1 >> find_chunks'2 offset acc
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
      return . Just . WEN_DUB $ \iter_dub -> do
        Iter.seek (8 + fromIntegral offset)
        let iter = convStream (conv_func fmt) iter_dub
        joinI $ joinI $ Iter.takeR count ==<< iter
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

unroll_8 :: (Monad m) => IterateeGM V Word8 m (Maybe (V Word8))
unroll_8 = liftI $ Iter.Cont step
  where
  step (Chunk vec)
    | Vec.null vec = unroll_8
    | True         = liftI $ Iter.Done (Just vec) (Chunk Vec.empty)
  step stream      = liftI $ Iter.Done Nothing stream

-- When unrolling to a Word8, use the specialized unroll_8 function
-- because we actually don't need to do anything
{-# RULES "unroll_8" forall n. unroll_n n = unroll_8 #-}
unroll_n :: (Storable a, MonadIO m) =>
            Int ->
            IterateeGM V Word8 m (Maybe (V a))
unroll_n wSize = liftI $ Iter.Cont step
  where
  step (Chunk vec)
    | Vec.null vec                    = unroll_n wSize
    | Vec.length vec < wSize          = liftI $ Iter.Cont $ step' vec
    | Vec.length vec `rem` wSize == 0 = liftIO (convert_vec vec) >>= \v ->
                                        liftI $ Iter.Done v (Chunk Vec.empty)
    | True    = let newLen = (Vec.length vec `div` wSize) * wSize
                    (h, t) = Vec.splitAt newLen vec
                in
                liftIO (convert_vec h) >>= \v -> liftI $ Iter.Done v (Chunk t)
  step stream = liftI $ Iter.Done Nothing stream
  step' i (Chunk vec)
    | Vec.null vec                          = liftI $ Iter.Cont $ step' i
    | Vec.length vec + Vec.length i < wSize = liftI $ Iter.Cont $ step'
                                              (Vec.append i vec)
    | True        = let vec' = Vec.append i vec
                        newLen = (Vec.length vec' `div` wSize) * wSize
                        (h, t) = Vec.splitAt newLen vec'
                    in
                    liftIO (convert_vec $ Vec.append i h) >>= \v ->
                      liftI $ Iter.Done v (Chunk t)
  step' _i stream = liftI $ Iter.Done Nothing stream
  convert_vec vec = let (fp, off, len) = VB.toForeignPtr vec
                        f = FP.plusPtr (FFP.unsafeForeignPtrToPtr fp) off
                    in
                    do
                    newFp <- FFP.newForeignPtr_ f
                    let newV = VB.fromForeignPtr (FFP.castForeignPtr newFp)
                               (len `div` wSize)
                    v' <- host_to_le newV
                    return $ Just v'

host_to_le :: Storable a => V a -> IO (V a)
host_to_le vec = do
  be' <- be
  case be' of
    True -> let
              (fp, off, len) = VB.toForeignPtr vec
              wSize = sizeOf $ Vec.head vec
            in
            loop wSize fp len off
    False -> return vec
    where
      loop _wSize _fp 0 _off = return vec
      loop wSize fp len off  = do
        FFP.withForeignPtr fp (\p -> swap_bytes wSize (p `FP.plusPtr` off))
        loop wSize fp (len - 1) (off + 1)

swap_bytes :: Int -> FP.Ptr a -> IO ()
swap_bytes wSize p = case wSize of
                          1 -> return ()
                          2 -> do
                               w1 <- (peekByteOff p 0) :: IO Word8
                               w2 <- (peekByteOff p 1) :: IO Word8
                               pokeByteOff p 0 w2
                               pokeByteOff p 1 w1
                          3 -> do
                               w1 <- (peekByteOff p 0) :: IO Word8
                               w3 <- (peekByteOff p 2) :: IO Word8
                               pokeByteOff p 0 w3
                               pokeByteOff p 1 w1
                          4 -> do
                               w1 <- (peekByteOff p 0) :: IO Word8
                               w2 <- (peekByteOff p 1) :: IO Word8
                               w3 <- (peekByteOff p 2) :: IO Word8
                               w4 <- (peekByteOff p 3) :: IO Word8
                               pokeByteOff p 0 w4
                               pokeByteOff p 1 w3
                               pokeByteOff p 2 w2
                               pokeByteOff p 3 w1
                          x -> do
                               let ns = [0..(x-1)]
                               ws <- sequence
                                     [(peekByteOff p n) :: IO Word8 | n <- ns]
                               sequence_ [ pokeByteOff p n w | n <- ns, w <- reverse ws]
                               return ()

-- |Convert Word8s to Doubles
conv_func :: (MonadIO m, Functor m) =>
             AudioFormat ->
             IterateeGM V Word8 m (Maybe (V Double))
conv_func (AudioFormat _nc _sr 8) = (fmap . fmap . Vec.map)
  (normalize 8 . (fromIntegral :: Word8 -> Int8)) unroll_8
conv_func (AudioFormat _nc _sr 16) = (fmap . fmap . Vec.map)
  (normalize 16 . (fromIntegral :: Word16 -> Int16))
  (unroll_n $ sizeOf (undefined :: Word16))
conv_func (AudioFormat _nc _sr 24) = (fmap . fmap . Vec.map)
  (normalize 24 . (fromIntegral :: Word32 -> Int32))
  (unroll_n 3)
conv_func (AudioFormat _nc _sr 32) = (fmap . fmap . Vec.map)
  (normalize 32 . (fromIntegral :: Word32 -> Int32))
  (unroll_n $ sizeOf (undefined :: Word32))
conv_func _ = Iter.iterErr "Invalid wave bit depth" >> return Nothing

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


-- ---------------------
-- convenience functions

-- |Convert (Maybe []) to [].  Nothing maps to an empty list.
join_m :: Maybe [a] -> [a]
join_m Nothing = []
join_m (Just a) = a

-- |Normalize a given value for the provided bit depth.
normalize :: Integral a => BitDepth -> a -> Double
normalize 8 a = (fromIntegral a - 128) / 128
normalize bd a = case (a > 0) of
  True ->  fromIntegral a / divPos
  False -> fromIntegral a / divNeg
  where
    divPos = fromIntegral (1 `shiftL` fromIntegral (bd - 1) :: Int) - 1
    divNeg = fromIntegral (1 `shiftL` fromIntegral (bd - 1) :: Int)

