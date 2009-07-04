module Sound.Iteratee.Codecs.Common (
  stringRead4,
  joinMaybe,
  convFunc
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
import Control.Monad
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
stringRead4 :: Monad m => IterateeG V Word8 m (String)
stringRead4 = do
  s1 <- Iter.head
  s2 <- Iter.head
  s3 <- Iter.head
  s4 <- Iter.head
  return $ map (chr . fromIntegral) [s1, s2, s3, s4]

unroll8 :: (Monad m) => IterateeG V Word8 m (Maybe (V Word8))
unroll8 = IterateeG step
  where
  step (Chunk vec)
    | Vec.null vec = return $ Cont unroll8 Nothing
    | True         = return $ Done (Just vec) (Chunk Vec.empty)
  step stream      = return $ Done Nothing stream

-- When unrolling to a Word8, use the specialized unroll8 function
-- because we actually don't need to do anything
{-# RULES "unroll8" forall n. unroller n = unroll8 #-}
unroller :: (Storable a, MonadIO m) =>
            Int ->
            IterateeG V Word8 m (Maybe (V a))
unroller wSize = IterateeG step
  where
  step (Chunk vec)
    | Vec.null vec           = return $ Cont (unroller wSize) Nothing
    | Vec.length vec < wSize = return $ Cont (IterateeG $ step' vec) Nothing
    | Vec.length vec `rem` wSize == 0
                             = liftIO $ liftM (flip Done (Chunk Vec.empty))
                                              (convert_vec vec)
  step (Chunk vec)           =
                let newLen = (Vec.length vec `div` wSize) * wSize
                    (h, t) = Vec.splitAt newLen vec
                in
                liftIO $ liftM (flip Done (Chunk t)) (convert_vec h)
  step stream = return $ Done Nothing stream
  step' i (Chunk vec)
    | Vec.null vec = return $ Cont (IterateeG $ step' i) Nothing
    | Vec.length vec + Vec.length i < wSize
                   = return $ Cont
                              (IterateeG $ step' (Vec.append i vec))
                              Nothing
    | True         = let vec' = Vec.append i vec
                         newLen = (Vec.length vec' `div` wSize) * wSize
                         (h, t) = Vec.splitAt newLen vec'
                     in
                     liftIO $ liftM (flip Done (Chunk t)) (convert_vec h)
  step' _i stream  = return $ Done Nothing stream
  convert_vec vec  = let (fp, off, len) = VB.toForeignPtr vec
                         f = FP.plusPtr (FFP.unsafeForeignPtrToPtr fp) off
                     in
                     do
                     newFp <- FFP.newForeignPtr_ f
                     let newV = VB.fromForeignPtr (FFP.castForeignPtr newFp)
                                (len `div` wSize)
                     v' <- hostToLE newV
                     return $ Just v'

hostToLE :: Storable a => V a -> IO (V a)
hostToLE vec = do
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
        FFP.withForeignPtr fp (swapBytes wSize . flip FP.plusPtr off)
        loop wSize fp (len - 1) (off + 1)

swapBytes :: Int -> FP.Ptr a -> IO ()
swapBytes wSize p = case wSize of
                          1 -> return ()
                          2 -> do
                               w1 <- peekByteOff p 0 :: IO Word8
                               w2 <- peekByteOff p 1 :: IO Word8
                               pokeByteOff p 0 w2
                               pokeByteOff p 1 w1
                          3 -> do
                               w1 <- peekByteOff p 0 :: IO Word8
                               w3 <- peekByteOff p 2 :: IO Word8
                               pokeByteOff p 0 w3
                               pokeByteOff p 1 w1
                          4 -> do
                               w1 <- peekByteOff p 0 :: IO Word8
                               w2 <- peekByteOff p 1 :: IO Word8
                               w3 <- peekByteOff p 2 :: IO Word8
                               w4 <- peekByteOff p 3 :: IO Word8
                               pokeByteOff p 0 w4
                               pokeByteOff p 1 w3
                               pokeByteOff p 2 w2
                               pokeByteOff p 3 w1
                          x -> do
                               let ns = [0..(x-1)]
                               ws <- sequence
                                     [peekByteOff p n :: IO Word8 | n <- ns]
                               sequence_ [ pokeByteOff p n w | n <- ns, w <- reverse ws]
                               return ()

-- |Convert Word8s to Doubles
convFunc :: (MonadIO m, Functor m) =>
             AudioFormat ->
             IterateeG V Word8 m (Maybe (V Double))
convFunc (AudioFormat _nc _sr 8) = (fmap . fmap . Vec.map)
  (normalize 8 . (fromIntegral :: Word8 -> Int8)) unroll8
convFunc (AudioFormat _nc _sr 16) = (fmap . fmap . Vec.map)
  (normalize 16 . (fromIntegral :: Word16 -> Int16))
  (unroller $ sizeOf (undefined :: Word16))
convFunc (AudioFormat _nc _sr 24) = (fmap . fmap . Vec.map)
  (normalize 24 . (fromIntegral :: Word24 -> Int24))
  (unroller $ sizeOf (undefined :: Word24))
convFunc (AudioFormat _nc _sr 32) = (fmap . fmap . Vec.map)
  (normalize 32 . (fromIntegral :: Word32 -> Int32))
  (unroller $ sizeOf (undefined :: Word32))
convFunc _ = throwErr (Err "Invalid wave bit depth")


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

