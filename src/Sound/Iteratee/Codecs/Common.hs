{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-# OPTIONS_GHC -Wall #-}
module Sound.Iteratee.Codecs.Common (
  joinMaybe

 ,RawFormattedChunk(..)
 ,NormFormattedChunk(..)
 ,nfChunkData
 ,convTrans
 ,quantizeRaw
 ,chunkFactor
)
  
where

import           Sound.Iteratee.Base
import qualified Data.Vector.Storable as V
import           Foreign.ForeignPtr
import           Foreign.Marshal.Utils (with)
import           Foreign.Storable
import           System.IO.Unsafe (unsafePerformIO)
import           Data.Bits
import           Data.Int
import           Data.Int.Int24
import           IterX.Fusion
import           Data.Monoid
import           Data.Word
import           Data.Word.Word24
import           Data.ListLike.Vector.Storable ()


-- =====================================================
-- useful type synonyms

-- determine host endian-ness
be :: IO Bool
be = fmap (==1) $ with (1 :: Word16) (\p -> peekByteOff p 1 :: IO Word8)

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


data RawFormattedChunk =
    RawFormattedChunk !AudioFormat !(V.Vector Word8)

data NormFormattedChunk =
    NormFormattedChunk !AudioFormat !(V.Vector Double)

nfChunkData :: NormFormattedChunk -> V.Vector Double
nfChunkData (NormFormattedChunk _ v) = v

chunkFactor :: AudioFormat -> Int
chunkFactor (AudioFormat nc _ bd) = nc * (bd `div` 8)

-- align raw chunks to bitdepth/channel boundaries
quantizeRaw :: Monad m => Transform' m RawFormattedChunk RawFormattedChunk
quantizeRaw = mealy f V.empty
  where
    f pre (RawFormattedChunk af v) =
      let len' = V.length v + V.length pre
      in case len' `rem` chunkFactor af of
          0 -> let !v' = pre <> v
               in if V.null v'
                    then (pre,[])
                    else (pre,[RawFormattedChunk af v'])
          n -> case len' > chunkFactor af of
              True  -> let !v' = pre <> v
                           !h  = V.unsafeTake (len'-n) v'
                           !r  = V.unsafeDrop (len'-n) v'
                       in (r, [RawFormattedChunk af h])
              False -> (pre<>v,[])

convTrans :: (Monad m, Functor m) => Transform' m RawFormattedChunk NormFormattedChunk
convTrans = quantizeRaw . unsafeConvTrans

-- |Convert Word8s to Doubles
-- This function should only be applied after 'quantizeRaw', or if the
-- vectors are otherwise known to be sized appropriately
unsafeConvTrans :: (Monad m, Functor m) =>
  Transform' m RawFormattedChunk NormFormattedChunk
unsafeConvTrans = maps (\(RawFormattedChunk af v) -> f af (bitDepth af) v)
  where
    f af 8 = NormFormattedChunk af .
      V.map (normalize 8 . (fromIntegral :: Word8 -> Int8)) . hostToLE
    f af 16 = NormFormattedChunk af .
      V.map (normalize 16 . (fromIntegral :: Word16 -> Int16)) . hostToLE
    f af 24 = NormFormattedChunk af .
      V.map (normalize 24 . (fromIntegral :: Word24 -> Int24)) . hostToLE
    f af 32 = NormFormattedChunk af .
      V.map (normalize 32 . (fromIntegral :: Word32 -> Int32)) . hostToLE
    f af _ = error $ "Invalid wave bit depth " ++ show af
