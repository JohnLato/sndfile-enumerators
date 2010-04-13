module Sound.Iteratee.Codecs.Common (
  stringRead4,
  joinMaybe,
  convFunc
)
  
where

import Sound.Iteratee.Base
import qualified Data.Iteratee as I
import Data.MutableIter as Iter
import qualified Data.MutableIter.IOBuffer as IB
import Data.MutableIter.IOBuffer (IOBuffer)
import Foreign.ForeignPtr
import Foreign.Storable
import qualified Foreign.Marshal.Utils as FMU
import Control.Monad.CatchIO
import Control.Monad.Trans
import Data.Char (chr)
import Data.Int.Int24
import Data.Int
import Data.Word
import Data.Word.Word24
import Data.Bits (shiftL)
import System.IO.Unsafe

-- =====================================================
-- useful type synonyms

type IOB m el = IOBuffer m el

-- determine host endian-ness
be :: IO Bool
be = fmap (==1) $ FMU.with (1 :: Word16) (\p -> peekByteOff p 1 :: IO Word8)

-- convenience function to read a 4-byte ASCII string
stringRead4 :: MonadCatchIO m => MIteratee (IOB r Word8) m (String)
stringRead4 = do
  s1 <- Iter.head
  s2 <- Iter.head
  s3 <- Iter.head
  s4 <- Iter.head
  return $ map (chr . fromIntegral) [s1, s2, s3, s4]

unroll8 :: (MonadCatchIO m) => MIteratee (IOB r Word8) m (Maybe (IOB r Word8))
unroll8 = liftI step
  where
  step (I.Chunk buf) = guardNull buf (liftI step) $
                         idone (Just buf) (I.Chunk IB.empty)
  step stream        = idone Nothing stream

-- When unrolling to a Word8, use the specialized unroll8 function
-- because we actually don't need to do anything
{-# RULES "unroll8" forall n. unroller n = unroll8 #-}
unroller :: (Storable a, MonadCatchIO m) =>
  Int
  -> MIteratee (IOB r Word8) m (Maybe (IOB r a))
unroller wSize = liftI step
  where
  step (I.Chunk buf) = guardNull buf (liftI step) $ do
    len <- liftIO $ IB.length buf
    if len < wSize then liftI (step' buf)
      else if len `rem` wSize == 0
              then do
                buf' <- liftIO $ convert_vec buf
                idone (Just buf') (I.Chunk IB.empty)
              else let newLen = (len `div` wSize) * wSize
                   in do
                      (h, t) <- liftIO $ IB.splitAt buf newLen
                      h' <- liftIO $ convert_vec h
                      idone (Just h') (I.Chunk t)
  step stream = idone Nothing stream
  step' i (I.Chunk buf) = guardNull buf (liftI (step' i)) $ do
    l <- liftIO $ IB.length buf
    iLen <- liftIO $ IB.length i
    newbuf <- liftIO $ IB.append i buf
    if l+iLen < wSize then liftI (step' newbuf)
       else do
         newLen <- liftIO $ IB.length newbuf
         let newLen' = (newLen `div` wSize) * wSize
         (h,t) <- liftIO $ IB.splitAt newbuf newLen'
         h' <- liftIO $ convert_vec h
         idone (Just h') (I.Chunk t)
  step' _i stream  = idone Nothing stream
  convert_vec vec  = IB.castBuffer vec >>= hostToLE

hostToLE :: (Monad m, Storable a) => IOB r a -> m (IOB r a)
hostToLE vec = let be' = unsafePerformIO be in case be' of
    True -> error "wrong endian-ness.  Ask the maintainer to implement hostToLE"
{-
              (fp, off, len) = VB.toForeignPtr vec
              wSize = sizeOf $ Vec.head vec
            in
            loop wSize fp len off
-}
    False -> return vec
{-
    where
      loop _wSize _fp 0 _off = return vec
      loop wSize fp len off  = do
        FFP.withForeignPtr fp (swapBytes wSize . flip FP.plusPtr off)
        loop wSize fp (len - 1) (off + 1)
-}

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
w16 :: Word16
w16 = 0
w24 :: Word24
w24 = 0
w32 :: Word32
w32 = 0

-- |Convert Word8s to Doubles
convFunc :: (MonadCatchIO m) =>
  AudioFormat
  -> ForeignPtr Int
  -> ForeignPtr Double
  -> MIteratee (IOBuffer r Word8) m (IOBuffer r Double)
convFunc (AudioFormat _nc _sr 8) offp bufp = do
  mbuf <- unroll8
  liftIO $ maybe (error "error in convFunc") (IB.mapBuffer
    (normalize 8 . (fromIntegral :: Word8 -> Int8)) offp bufp) mbuf
convFunc (AudioFormat _nc _sr 16) offp bufp = do
  mbuf <- unroller (sizeOf w16)
  liftIO $ maybe (error "error in convFunc") (IB.mapBuffer
    (normalize 16 . (fromIntegral :: Word16 -> Int16)) offp bufp) mbuf
convFunc (AudioFormat _nc _sr 24) offp bufp = do
  mbuf <- unroller (sizeOf w24)
  liftIO $ maybe (error "error in convFunc") (IB.mapBuffer
    (normalize 24 . (fromIntegral :: Word24 -> Int24)) offp bufp) mbuf
convFunc (AudioFormat _nc _sr 32) offp bufp = do
  mbuf <- unroller (sizeOf w32)
  liftIO $ maybe (error "error in convFunc") (IB.mapBuffer
    (normalize 32 . (fromIntegral :: Word32 -> Int32)) offp bufp) mbuf
convFunc _ _ _ = MIteratee $ I.throwErr (I.iterStrExc "Invalid wave bit depth")


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

