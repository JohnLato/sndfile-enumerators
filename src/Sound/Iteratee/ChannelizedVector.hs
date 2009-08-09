{-# LANGUAGE MultiParamTypeClasses,
             FlexibleInstances,
             FlexibleContexts,
             GeneralizedNewtypeDeriving,
             ScopedTypeVariables #-}

module Sound.Iteratee.ChannelizedVector (
  Channelized (..)
  ,ZipListS (..)
  ,Mono (..)
  ,Pair (..)
  ,ChannelizedVector
  ,numChannels
  ,interleave
  ,channelize
  ,numFrames
  ,index
  ,getChannelV
  ,getFrame
  ,take
  ,drop
  ,splitAt
  ,mapAll
  ,mapChannels
  ,mapChannelV
  ,foldl
  ,foldl'
)

where

import Prelude hiding (take, drop, splitAt, foldl)
import qualified Prelude as P

import Sound.Iteratee.Base
import qualified Data.StorableVector as SV
import Foreign.Storable
import qualified Data.TypeLevel.Num as T
import qualified Data.IntMap as IM

import Control.Arrow
import Control.Applicative

type V = SV.Vector

type ChannelIndex = Int

-- helper function
fI :: (Integral a, Num b) => a -> b
fI = fromIntegral

-- -------------------------------------------------------
-- Support for channelized operations

-- |Containers that are useful for holding multi-channel stuff.
-- N.B. for good performance, mapChannel should be strict.
class (T.Nat s) => Channelized s c el where
  getChannel  :: (c s el) -> ChannelIndex -> el
  setChannel  :: ChannelIndex -> el -> (c s el) -> (c s el)
  mapChannel  :: (el -> el) -> ChannelIndex -> (c s el) -> (c s el)

newtype T.Nat s => ZipListS s a = ZipListS {getZipListS :: [a]}
  deriving (Eq, Functor, Show)

instance T.Nat s => Applicative (ZipListS s) where
  pure    = pureFn
  a <*> b = ZipListS $ zipWith ($) (getZipListS a) (getZipListS b)

pureFn :: forall s a. (T.Nat s) => a -> ZipListS s a
pureFn = ZipListS . replicate (T.toInt (undefined :: s))

instance T.Nat s => Channelized s ZipListS el where
  getChannel z i = getZipListS z !! (fI i)
  setChannel i el = ZipListS . uncurry (++) . ((++ [el]) *** P.drop 1) .
                        P.splitAt (fI i) . getZipListS
  mapChannel f i  = ZipListS . uncurry (++) . second (\(x:xs) -> f x : xs) .
                        P.splitAt (fI i) . getZipListS

data Pair s a = Pair a a deriving (Eq, Show)

instance Functor (Pair s) where
  fmap f (Pair a1 a2) = Pair (f a1) (f a2)

instance Applicative (Pair T.D2) where
  pure a = Pair a a
  (Pair a1 a2) <*> (Pair b1 b2) = Pair (a1 b1) (a2 b2) 

instance Channelized T.D2 Pair el where
  getChannel (Pair a _) 0 = a
  getChannel (Pair _ b) 1 = b
  getChannel _          _ = error "getChannel Pair invalid index"
  {-# INLINE getChannel #-}

  setChannel 0 a (Pair _ b) = Pair a b
  setChannel 1 b (Pair a _) = Pair a b
  setChannel _ _ _          = error "setChannel Pair invalid index"
  {-# INLINE setChannel #-}

  mapChannel f 0 (Pair a b) = let a' = f a in a' `seq` Pair a' b
  mapChannel f 1 (Pair a b) = let b' = f b in b' `seq` Pair a b'
  mapChannel _ _ _          = error "setChannel Pair invalid index"
  {-# INLINE mapChannel #-}

newtype T.Nat s => Mono s a = Mono a deriving (Eq, Show)

instance Functor (Mono s) where
  fmap f (Mono a) = Mono (f a)

instance Applicative (Mono T.D1) where
  pure a = Mono a
  (Mono a) <*> (Mono b) = Mono (a b)

instance Channelized T.D1 Mono el where
  getChannel (Mono a) 0 = a
  getChannel _         _ = error "getChannel Mono invalid index"
  {-# INLINE getChannel #-}

  setChannel 0 a (Mono _) = Mono a
  setChannel _ _ _          = error "setChannel Mono invalid index"
  {-# INLINE setChannel #-}

  mapChannel f 0 (Mono a) = Mono (f a)
  mapChannel _ n _          = error $ "mapChannel Mono invalid index " ++ show n
  {-# INLINE mapChannel #-}

  fromList (a:_) = Mono a

-- -------------------------------------------------------
-- Support for channelized vectors

-- |Create a channelized vector from an interleaved vector
data ChannelizedVector a = CVec !NumChannels !(V a)

numChannels :: ChannelizedVector a -> NumChannels
numChannels (CVec nc _) = nc

interleave :: ChannelizedVector a -> V a
interleave (CVec _ v) = v

-- |Create a ChannelizedVector.  The vector length
-- must be a multiple of the number of channels requested.
channelize :: NumChannels -> V a -> Maybe (ChannelizedVector a)
channelize 1 v = Just (CVec 1 v)
channelize n v = if (SV.length v `rem` (fI n) == 0) && (n > 0)
  then Just (CVec n v)
  else Nothing

-- |Make a copy of the specified channel from a ChannelizedVector.
getChannelV :: Storable a => ChannelizedVector a -> ChannelIndex -> V a
getChannelV cv n = SV.sample (fI $ numFrames cv) (\i -> index cv (fI i) n)

-- |Return a frame of data
getFrame :: Storable a => ChannelizedVector a -> FrameCount -> [a]
getFrame cv@(CVec nc _) fc = map (index cv fc) [0 .. (fromIntegral $ nc - 1)]

rawPosition :: NumChannels -> FrameCount -> ChannelIndex -> Int
rawPosition nc fc c = (fI nc * fI fc) + (fI c)

-- |Calculate the number of frames in a ChannelizedVector
numFrames :: ChannelizedVector a -> FrameCount
numFrames = fI . uncurry (*) .
            (fI . numChannels &&& (SV.length . interleave))

-- |Look up a value in a ChannelizedVector by frame and channel.
index :: (Storable a) => ChannelizedVector a -> FrameCount -> ChannelIndex -> a
index (CVec nc v) fc c = SV.index v (rawPosition nc fc c)

-- --------------------------------------------
-- Basic interface

-- |Take n frames from a ChannelizedVector.
take :: (Storable a) => ChannelizedVector a -> FrameCount -> ChannelizedVector a
take (CVec nc v) fc = CVec nc $ SV.take (fI nc * fI fc) v

-- |Drop n frames from a ChannelizedVector.
drop :: (Storable a) => ChannelizedVector a -> FrameCount -> ChannelizedVector a
drop (CVec nc v) fc = CVec nc $ SV.drop (fI nc * fI fc) v

-- |Split a ChannelizedVector into two at the given frame position.
splitAt :: (Storable a) => ChannelizedVector a
  -> FrameCount
  -> (ChannelizedVector a, ChannelizedVector a)
splitAt (CVec nc v) fc = (CVec nc v1, CVec nc v2)
  where
    (v1, v2) = SV.splitAt (fI nc * fI fc) v

-- --------------------------------------------
-- Maps

-- |Map a function uniformly over all channels.
-- O(n)
mapAll :: Storable a => (a -> a)
  -> ChannelizedVector a
  -> ChannelizedVector a
mapAll f (CVec n v) = CVec n $ SV.map f v

-- |Map a list of functions, one per channel, over a ChannelizedVector.
-- O(n)
mapChannels :: (Channelized s c (a -> a), Storable a) => c s (a -> a)
  -> ChannelizedVector a
  -> ChannelizedVector a
mapChannels fs (CVec nc v) = CVec nc $ SV.mapIndexed f v
  where
    f i = fs `getChannel` (normChn nc i)

{- --I'd like to test this version, but mapIndexed may be a better choice.
mapChannels fs cv@(CVec nc v) = CVec nc $
  SV.sample (fI $ numFrames cv) (\i ->
    f (normChn nc i) (SV.index v i))
  where
    f n = fs !! n
-}

-- |Map a function over one channel of a ChannelizedVector.
-- O(n), where n is the total length of the ChannelizedVector.
mapChannelV :: Storable a => (a -> a)
  -> ChannelIndex
  -> ChannelizedVector a
  -> ChannelizedVector a
mapChannelV f n (CVec nc v) = CVec nc $ SV.mapIndexed f' v
  where
    f' i = if (normChn nc i) == fI n then f else id

-- ----------------
-- folds
-- I'm not doing many folds yet, we'll see what I need.

-- |Perform a fold over each channel
foldl :: Storable a => [(b -> a -> b)] -> [b] -> ChannelizedVector a -> [b]
foldl fs i0s (CVec nc v0)
  | nc == 1 = [SV.foldl (head fs) (head i0s) v0]
  | otherwise = fmap snd $ SV.foldl f (P.take (fI nc) $ P.zip fs i0s) v0
  where
    f ((f1, acc):accs) b = let newacc = f1 acc b in accs ++ [(f1, newacc)]

-- In addition to the applicative version, I could also try a mapAccum
-- implementation, or a lower-level version.

foldl' :: (Channelized s c (a -> b -> a),
           Channelized s c a,
           Storable b) =>
  c s (a -> b -> a)
  -> c s a
  -> ChannelizedVector b
  -> c s a
foldl' fs i0s (CVec nc v0)
  | nc == 1 = setChannel 0 (SV.foldl' (getChannel fs 0) (getChannel i0s 0) v0)
                         i0s
foldl' fs i0s (CVec nc v0) = sndS $ SV.foldl' f acc0 v0
  where
    acc0 = PairS 0 i0s
    getF n = getChannel fs n
    succ' n = normChn nc (n + 1)
    f (PairS n is) b = let f' = flip (getF n) b
                       in PairS (succ' n) $ mapChannel f' n is
{-# INLINE foldl' #-}

-- A strict pair type
data PairS a b = PairS !a !b

fstS :: PairS a b -> a
fstS (PairS a _) = a

sndS :: PairS a b -> b
sndS (PairS _ b) = b

pApply :: PairS (b -> c) b -> c
pApply (PairS f b) = f b

setSnd :: PairS a b -> b -> PairS a b
setSnd (PairS a _) b' = PairS a b'

-- Helper function for calculating channels
normChn :: NumChannels -> ChannelIndex -> ChannelIndex
normChn 1 _  = 0
normChn nc i = i `rem` (fI nc)
