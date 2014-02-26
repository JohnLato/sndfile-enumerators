{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

{-# OPTIONS_GHC -Wall #-}
module Sound.Iteratee.Codecs.Common (
  joinMaybe

 ,RawFormattedChunk(..)
 ,NormFormattedChunk(..)
 ,nfChunkData
 ,convTrans
 ,convTrans2
 ,quantizeRaw
 ,chunkFactor
)
  
where

import           Sound.Iteratee.Base
import qualified Data.Vector.Storable as V
import           Control.Monad.IO.Class
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
{-# INLINE normalize #-}

data RawFormattedChunk =
    RawFormattedChunk !AudioFormat !(V.Vector Word8)

data NormFormattedChunk =
    NormFormattedChunk !AudioFormat !(V.Vector Double)

nfChunkData :: NormFormattedChunk -> V.Vector Double
nfChunkData (NormFormattedChunk _ v) = v

chunkFactor :: AudioFormat -> Int
chunkFactor (AudioFormat nc _ bd) = nc * (bd `shiftR` 3)

-- align raw chunks to bitdepth boundaries
quantizeRaw :: Monad m => Transform' m RawFormattedChunk RawFormattedChunk
quantizeRaw = mealy f V.empty . filterMaybe
  where
    f pre (RawFormattedChunk af v) =
        let !len' = V.length v'
            !v' = if V.null pre then v else pre <> v
        in case len' `rem` chunkFactor af of
            0 -> if V.null v'
                      then (v',Nothing)
                      else (V.empty,Just (RawFormattedChunk af v'))
            n -> case len' > chunkFactor af of
                True  -> let !h  = V.unsafeTake (len'-n) v'
                             !r  = V.unsafeDrop (len'-n) v'
                         in (r, Just (RawFormattedChunk af h))
                False -> (v',Nothing)
{-# INLINE quantizeRaw #-}

convTrans :: (MonadIO m, Functor m) => Transform' m RawFormattedChunk NormFormattedChunk
convTrans = quantizeRaw . unsafeConvTrans
{-# INLINE convTrans #-}

{-# INLINE convTrans2 #-}
convTrans2 :: (MonadIO m) => AudioFormat -> Transform' m (V.Vector Word8) NormFormattedChunk
convTrans2 af = unfRaw af . maps (NormFormattedChunk af)

{-# INLINE [1] unfRaw #-}
unfRaw :: MonadIO m => AudioFormat -> Transform' m (V.Vector Word8) (V.Vector Double)
unfRaw af = case bitDepth af of
    8  -> foldUnfolding unfoldVec . maps (fromIntegral :: Word8 -> Int8) . maps (normalize 8) . foldVec osize
    -- the uf16 code is pretty slow, and the unfoldUf16 is worse than the
    -- original convTrans.  Although uf16 should be pretty much the same as
    -- convTrans, so maybe I just need to find a way to speed up umealy?
    -- 16 -> uf16 . maps (normalize 16) . foldVec osize
    16 -> foldUnfolding unfoldUf16 . maps (normalize 16 :: Int16 -> Double) . foldVec osize
    24 -> foldUnfolding unfoldVec . maps fromIntegral . fold24 . maps (normalize 24) . foldVec osize
  where
    osize = 4096

{-# INLINE unfoldUf16 #-}
unfoldUf16 :: Monad m => UnfoldM m (V.Vector Word8) Int16
unfoldUf16 = UnfoldM mk f
  where
    mk vec | V.length vec `rem` 2 == 0 = case V.unsafeToForeignPtr vec of
              (p,a,b) -> return $ V.unsafeFromForeignPtr (castForeignPtr p :: ForeignPtr Int16) a (b `shiftR` 1)
           | otherwise = error "extras"

    {-# INLINE f #-}
    f v | not (V.null v) = return $ UnfoldStep (V.unsafeHead v) (V.unsafeDrop 1 v)
        | otherwise = unfoldDone

{-# INLINE [1] uf16 #-}
uf16 :: Monad m => Transform' m (V.Vector Word8) Int16
uf16 = umealy f S1_0'
  where
    f S1_0' vec
      | V.null vec = (S1_0', unfoldEmpty)
      | V.length vec `rem` 2 == 0 = (S1_0', ufVec vec)
      | otherwise = (S1_1' $ V.unsafeLast vec, ufVec $ V.unsafeInit vec)
    f' v | not (V.null v) = return $ UnfoldStep (V.unsafeHead v) (V.unsafeDrop 1 v)
         | otherwise = unfoldDone
    {-# INLINE ufVec #-}
    ufVec vec = UnfoldM (const $ return v') f'
        where
          {-# INLINE v' #-}
          v' :: V.Vector Int16
          v' = case V.unsafeToForeignPtr vec of
              (p,a,b) -> V.unsafeFromForeignPtr (castForeignPtr p) a (b `shiftR` 1)

data S1' = S1_0' | S1_1' !Word8

{-# INLINE fold16 #-}
fold16 :: Monad m => Transform' m Word8 Int16
fold16 = umealy f S1_0
  where
    f S1_0 !x = (S1_1 $ fromIntegral x,unfoldEmpty)
    f (S1_1 !y) !x = (S1_0, ) $! uReplicate 1 $! y + shiftL (fromIntegral x) 8

data S1 = S1_0 | S1_1 !Int16

{-# INLINE fold24 #-}
fold24 :: Monad m => Transform' m Int24 Int24
fold24 = umealy f (0::Int,0)
  where
    {-# INLINE f #-}
    f !(0,!_) !x = ((1,x),unfoldEmpty)
    f !(1,!y) !x = ((2,y + shiftL x 8), unfoldEmpty)
    f !(2,!y) !x = ((0,0), uReplicate 1 $! y + shiftL x 8)

{-# INLINE unsafeConvTrans #-}
-- |Convert Word8s to Doubles
-- This function should only be applied after 'quantizeRaw',
-- or if the vectors are otherwise known to be sized appropriately
unsafeConvTrans :: (Monad m, Functor m) =>
  Transform' m RawFormattedChunk NormFormattedChunk
unsafeConvTrans = maps (\(RawFormattedChunk af v) -> f af (bitDepth af) v)
  where
    {-# INLINE f #-}
    f af 8 = NormFormattedChunk af .
      V.map (normalize 8 . (fromIntegral :: Word8 -> Int8)) . hostToLE
    f af 16 = NormFormattedChunk af .
      V.map (normalize 16 . (fromIntegral :: Word16 -> Int16)) . hostToLE
    f af 24 = NormFormattedChunk af .
      V.map (normalize 24 . (fromIntegral :: Word24 -> Int24)) . hostToLE
    f af 32 = NormFormattedChunk af .
      V.map (normalize 32 . (fromIntegral :: Word32 -> Int32)) . hostToLE
    f af _ = error $ "Invalid wave bit depth " ++ show af
