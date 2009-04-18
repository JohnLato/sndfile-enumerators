{-# LANGUAGE RankNTypes #-}
module Sound.Iteratee.Codecs.Common (
  string_read4,
  join_m,
  normalize,
  unroll_n,
  conv_func
)
  
where

import Sound.Iteratee.Base
import qualified Data.Iteratee.Base as Iter
import Sound.Iteratee.Instances()
import Data.Iteratee.Base
import qualified Data.StorableVector as Vec
import qualified Data.StorableVector.Base as VB
import qualified Foreign.Ptr as FP
import qualified Foreign.ForeignPtr as FFP
import Foreign.Storable
import qualified Foreign.Marshal.Utils as FMU
import Control.Monad.Trans
import Data.Char (chr)
import Data.Int.Int24
import Data.Int
import Data.Word
import Data.Word.Word24
import Data.Bits (shiftL)
import System.IO

-- =====================================================
-- useful type synonyms

type V    = Vec.Vector

-- determine host endian-ness
be :: IO Bool
be = fmap (==1) $ FMU.with (1 :: Word16) (\p -> peekByteOff p 1 :: IO Word8)

-- convenience function to read a 4-byte ASCII string
string_read4 :: Monad m => IterateeGM V Word8 m (Maybe String)
string_read4 =
  bindm Iter.head $ \s1 ->
   bindm Iter.head $ \s2 ->
   bindm Iter.head $ \s3 ->
   bindm Iter.head $ \s4 ->
   return . Just $ map (chr . fromIntegral) [s1, s2, s3, s4]

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
  step (Chunk vec)
    | Vec.length vec < wSize          = liftI $ Iter.Cont $ step' vec
  step (Chunk vec)
    | Vec.length vec `rem` wSize == 0 = liftIO (convert_vec vec) >>= \v ->
                                        liftI $ Iter.Done v (Chunk Vec.empty)
  step (Chunk vec)                    =
                let newLen = (Vec.length vec `div` wSize) * wSize
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
                    liftIO (convert_vec h) >>= \v ->
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
  (normalize 24 . (fromIntegral :: Word24 -> Int24))
  (unroll_n $ sizeOf (undefined :: Word24))
conv_func (AudioFormat _nc _sr 32) = (fmap . fmap . Vec.map)
  (normalize 32 . (fromIntegral :: Word32 -> Int32))
  (unroll_n $ sizeOf (undefined :: Word32))
conv_func _ = Iter.iterErr "Invalid wave bit depth" >> return Nothing

-- ---------------------
-- convenience functions

-- |Convert (Maybe []) to [].  Nothing maps to an empty list.
join_m :: Maybe [a] -> [a]
join_m Nothing = []
join_m (Just a) = a

-- |Normalize a given value for the provided bit depth.
-- This uses wave-standard normalization.  I'll support more formats
-- if/when it becomes necessary.
normalize :: Integral a => BitDepth -> a -> Double
normalize 8 a = (fromIntegral a - 128) / 128
normalize bd a = case (a > 0) of
  True ->  fromIntegral a / divPos
  False -> fromIntegral a / divNeg
  where
    divPos = fromIntegral (1 `shiftL` fromIntegral (bd - 1) :: Integer) - 1
    divNeg = fromIntegral (1 `shiftL` fromIntegral (bd - 1) :: Integer)

