{-# LANGUAGE ScopedTypeVariables #-}

module Sound.Iteratee.Codecs.Common (
  stringRead4
 ,joinMaybe
 ,convFunc
)
  
where

import Sound.Iteratee.Base
import           Data.Iteratee as I
import qualified Data.Vector.Storable as V
import Foreign
import Control.Monad (replicateM, liftM)
import Control.Monad.CatchIO
import Data.Char (chr)
import Data.Int.Int24
import Data.Word.Word24
import Data.ListLike.Vector.Storable ()

-- =====================================================
-- useful type synonyms

-- determine host endian-ness
be :: IO Bool
be = fmap (==1) $ with (1 :: Word16) (\p -> peekByteOff p 1 :: IO Word8)

-- convenience function to read a 4-byte ASCII string
stringRead4 :: MonadCatchIO m => Iteratee (V.Vector Word8) m String
stringRead4 = (liftM . map) (chr . fromIntegral) $ replicateM 4 I.head

unroll8 :: (MonadCatchIO m) => Iteratee (V.Vector Word8) m (Maybe (V.Vector Word8))
unroll8 = liftI step
  where
  step (I.Chunk buf)
    | V.null buf = liftI step
    | otherwise  = idone (Just buf) (I.Chunk V.empty)
  step stream        = idone Nothing stream

-- When unrolling to a Word8, use the specialized unroll8 function
-- because we actually don't need to do anything
{-# RULES "unroll8" forall n. unroller n = unroll8 #-}
unroller :: (Storable a, MonadCatchIO m) =>
  Int
  -> Iteratee (V.Vector Word8) m (Maybe (V.Vector a))
unroller wSize = liftI step
  where
  step (I.Chunk buf)
   | V.null buf = liftI step
   | otherwise = do
    let len = V.length buf
    if len < wSize
      then liftI $ step' buf
      else if len `rem` wSize == 0
              then do
                let buf' = convert_vec buf
                idone (Just buf') (I.Chunk V.empty)
              else let newLen = (len `div` wSize) * wSize
                       h      = convert_vec $ V.take newLen buf
                       t      = V.drop newLen buf
                   in do
                      idone (Just h) (I.Chunk t)
  step stream = idone Nothing stream
  step' i (I.Chunk buf)
   | V.null buf = liftI (step' i)
   | otherwise = do
    let l    = V.length buf
        iLen = V.length i
        newbuf = i V.++ buf
    if l+iLen < wSize then liftI (step' newbuf)
       else do
         let newLen  = V.length newbuf
             newLen' = (newLen `div` wSize) * wSize
             h       = convert_vec $ V.take newLen' newbuf
             t       = V.drop newLen' newbuf
         idone (Just h) (I.Chunk t)
  step' _i stream  = idone Nothing stream
  convert_vec = hostToLE

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
convFunc :: (MonadCatchIO m) =>
  AudioFormat
  -> Iteratee (V.Vector Word8) m (V.Vector Double)
convFunc (AudioFormat _nc _sr 8) = do
  mbuf <- unroll8
  return $ maybe (error "error in convFunc") (V.map
    (normalize 8 . (fromIntegral :: Word8 -> Int8))) mbuf
convFunc (AudioFormat _nc _sr 16) = do
  mbuf <- unroller (sizeOf w16)
  return $ maybe (error "error in convFunc") (V.map
    (normalize 16 . (fromIntegral :: Word16 -> Int16))) mbuf
convFunc (AudioFormat _nc _sr 24) = do
  mbuf <- unroller (sizeOf w24)
  return $ maybe (error "error in convFunc") (V.map
    (normalize 24 . (fromIntegral :: Word24 -> Int24))) mbuf
convFunc (AudioFormat _nc _sr 32) = do
  mbuf <- unroller (sizeOf w32)
  return $ maybe (error "error in convFunc") (V.map
    (normalize 32 . (fromIntegral :: Word32 -> Int32))) mbuf
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

