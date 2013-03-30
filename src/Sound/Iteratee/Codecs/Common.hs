{-# LANGUAGE ScopedTypeVariables #-}

module Sound.Iteratee.Codecs.Common (
  joinMaybe
 ,convFunc
)
  
where

import           Sound.Iteratee.Base
import           Data.Iteratee as I
import qualified Data.Vector.Storable as V
import           Foreign.ForeignPtr
import           Foreign.Marshal.Utils (with)
import           Foreign.Storable
import           System.IO.Unsafe (unsafePerformIO)
import           Control.Applicative
import           Data.Bits
import           Data.Int
import           Data.Int.Int24
import           Data.Word
import           Data.Word.Word24
import           Data.ListLike.Vector.Storable ()


-- =====================================================
-- useful type synonyms

-- determine host endian-ness
be :: IO Bool
be = fmap (==1) $ with (1 :: Word16) (\p -> peekByteOff p 1 :: IO Word8)

unroll8 :: (Monad m) => Iteratee (V.Vector Word8) m (V.Vector Word8)
unroll8 = I.getChunk

-- When unrolling to a Word8, use the specialized unroll8 function
-- because we actually don't need to do anything
{-# RULES "unroll8" forall n. unroller n = unroll8 #-}
unroller :: (Storable a, Monad m, Functor m) =>
  Int
  -> Iteratee (V.Vector Word8) m (V.Vector a)
unroller wSize = icont step
  where
  step NoData = continue step
  step (I.Chunk buf)
   | V.null buf = continue step
   | otherwise = do
    let len = V.length buf
    if len < wSize
      then continue $ step' buf
      else if len `rem` wSize == 0
              then do
                let buf' = hostToLE buf
                contDoneM buf' NoData
              else let newLen = (len `div` wSize) * wSize
                       h      = hostToLE $ V.take newLen buf
                       t      = V.drop newLen buf
                   in contDoneM h $ I.Chunk t
  step stream@(EOF{}) = contDoneM V.empty stream
  step' i NoData = continue (step' i)
  step' i (I.Chunk buf)
   | V.null buf = continue (step' i)
   | otherwise = do
    let l    = V.length buf
        iLen = V.length i
        newbuf = i V.++ buf
    if l+iLen < wSize
       then continue (step' newbuf)
       else step (I.Chunk newbuf)
  step' _i stream@(EOF{}) = contDoneM V.empty stream

hostToLE :: forall a. Storable a => V.Vector Word8 -> V.Vector a
hostToLE vec = let be' = unsafePerformIO be in if be'
    then error "wrong endian-ness.  Ask the maintainer to implement hostToLE"
{-
              (fp, off, len) = VB.toForeignPtr vec
              wSize = sizeOf $ Vec.head vec
            in
            loop wSize fp len off
-}
    else let (ptr, offset,len) = V.unsafeToForeignPtr vec
         in V.unsafeFromForeignPtr (castForeignPtr ptr)
                                   offset
                                   (len `quot` sizeOf (undefined :: a))

{-
swapBytes :: Int -> ForeignPtr a -> IO ()
swapBytes wSize fp = withForeignPtr fp $ \p -> case wSize of
  1 -> return ()
  2 -> do
    (w1 :: Word8) <- peekByteOff p 0
    (w2 :: Word8) <- peekByteOff p 1
    pokeByteOff p 0 w2
    pokeByteOff p 1 w1
  3 -> do
    (w1 :: Word8) <- peekByteOff p 0
    (w3 :: Word8) <- peekByteOff p 2
    pokeByteOff p 0 w3
    pokeByteOff p 2 w1
  4 -> do
    (w1 :: Word8) <- peekByteOff p 0
    (w2 :: Word8) <- peekByteOff p 1
    (w3 :: Word8) <- peekByteOff p 2
    (w4 :: Word8) <- peekByteOff p 3
    pokeByteOff p 0 w4
    pokeByteOff p 1 w3
    pokeByteOff p 2 w2
    pokeByteOff p 3 w1
  _ -> error "swapBytes called with wordsize > 4"

w8 :: Word8
w8 = 0
-}
w16 :: Word16
w16 = 0
w24 :: Word24
w24 = 0
w32 :: Word32
w32 = 0

-- |Convert Word8s to Doubles
convFunc :: (Monad m, Functor m) =>
  AudioFormat
  -> Iteratee (V.Vector Word8) m (V.Vector Double)
convFunc (AudioFormat _nc _sr 8) =
  V.map (normalize 8 . (fromIntegral :: Word8 -> Int8)) <$> unroll8
convFunc (AudioFormat _nc _sr 16) =
  V.map (normalize 16 . (fromIntegral :: Word16 -> Int16))
    <$> unroller (sizeOf w16)
convFunc (AudioFormat _nc _sr 24) =
  V.map (normalize 24 . (fromIntegral :: Word24 -> Int24))
    <$> unroller (sizeOf w24)
convFunc (AudioFormat _nc _sr 32) =
  V.map (normalize 32 . (fromIntegral :: Word32 -> Int32))
    <$> unroller (sizeOf w32)
convFunc _ = I.throwErr (I.iterStrExc "Invalid wave bit depth")


-- ---------------------
-- convenience functions

-- |Convert (Maybe []) to [].  Nothing maps to an empty list.
joinMaybe :: Maybe [a] -> [a]
joinMaybe Nothing = []
joinMaybe (Just a) = a

-- |Normalize a given value for the provided bit depth.
-- This uses wave-standard normalization.  I'll support more formats
-- if/when it becomes necessary.
normalize :: Integral a => BitDepth -> a -> Double
normalize 8 = \a -> let m = 1 / 128 in m * (fromIntegral a - 128)
normalize bd = \a -> if a > 0
                       then fromIntegral a * mPos
                       else fromIntegral a * mNeg
  where
    mPos = 1/ (fromIntegral (1 `shiftL` fromIntegral (bd - 1) :: Integer) - 1)
    mNeg = 1/ fromIntegral (1 `shiftL` fromIntegral (bd - 1) :: Integer)

