-- |This module provides support for channelized vector operations.

{-# LANGUAGE MultiParamTypeClasses,
             FlexibleInstances,
             FlexibleContexts,
             BangPatterns,
             GeneralizedNewtypeDeriving,
             ScopedTypeVariables,
             Rank2Types #-}

module Sound.Iteratee.ChannelizedVector (
  -- *Types
  -- ** Channelized type class instances
  ListVector
  ,Mono (..)
  ,Stereo (..)
  -- ** ChannelizedVector
  ,ChannelizedVector
  -- ** Classes
  ,Channelized (..)
  -- ** Other types
  ,ChannelIndex
  -- * Functions
  -- ** Basic interface
  ,numChannels
  ,numFrames
  ,index
  ,getChannelV
  ,getFrame
  -- ** Conversion to/from Vector
  ,interleave
  ,channelize
  ,channelizeT
  -- ** List-list operations
  ,take
  ,drop
  ,splitAt
  -- ** Maps
  ,mapAll
  ,mapChannels
  ,mapChannelV
  -- ** Folds
  ,foldl
  ,foldl'
  -- * Enumerator support
  ,interleaveStream
  ,channelizeStream
)

where

import Prelude hiding (take, drop, splitAt, foldl)
import qualified Prelude as P

import Sound.Iteratee.Base
import Data.Iteratee (IterateeG (..), EnumeratorN, IterGV (..), StreamG (..))
import qualified Data.Iteratee as I
import qualified Data.TypeLevel.Num as T
import qualified Data.StorableVector as SV
import qualified Data.StorableVector.Base as SVB

import Control.Arrow
import Control.Applicative
import Data.Monoid

import System.IO.Unsafe
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Storable

type V = SV.Vector

type ChannelIndex = Int

-- helper function
fI :: (Integral a, Num b) => a -> b
fI = fromIntegral

-- -------------------------------------------------------
-- Support for channelized operations

-- |Containers for multi-channel type operations.
-- s is a type-level indicator of the number of channels in the container.
-- For good performance, most Container types should be strict.
class (T.Nat s, Applicative (c s)) => Channelized s c el where
  getChannel :: (c s el) -> ChannelIndex -> el
  setChannel :: ChannelIndex -> el -> (c s el) -> (c s el)
  mapChannel :: (el -> el) -> ChannelIndex -> (c s el) -> (c s el)
  toC        :: [el] -> (c s el)
  fromC      :: (c s el) -> [el]

-- A ListVector is a List with a type-level length.
newtype T.Nat s => ListVector s a = ListVector {getListVector :: [a]}
  deriving (Eq, Functor, Show)

-- |Create a ListVector
makeListVector :: T.Nat s => s -> [a] -> ListVector s a
makeListVector s xs | T.toInt s < P.length xs =
  error $ "input list to short to make ListVector of length " ++
          (show $ T.toInt s)
makeListVector s xs = ListVector . P.take (T.toInt s) $ xs

instance T.Nat s => Applicative (ListVector s) where
  pure    = pureLV
  a <*> b = ListVector $ zipWith ($) (getListVector a) (getListVector b)

pureLV :: forall s a. (T.Nat s) => a -> ListVector s a
pureLV = ListVector . replicate (T.toInt (undefined :: s))

instance forall s el. (T.Nat s) => Channelized s ListVector el where
  getChannel z i = getListVector z !! (fI i)
  setChannel i el = ListVector . uncurry (++) . ((++ [el]) *** P.drop 1) .
                        P.splitAt (fI i) . getListVector
  mapChannel f i  = ListVector . uncurry (++) . second (\(x:xs) -> f x : xs) .
                        P.splitAt (fI i) . getListVector
  toC             = makeListVector (undefined :: s)
  fromC           = getListVector

-- |A Strict pair type.
data Stereo s a = Stereo !a !a deriving (Eq, Show)

instance Functor (Stereo s) where
  fmap f (Stereo a1 a2) = Stereo (f a1) (f a2)

instance Applicative (Stereo T.D2) where
  pure a = Stereo a a
  (Stereo a1 a2) <*> (Stereo b1 b2) = Stereo (a1 b1) (a2 b2) 

instance Channelized T.D2 Stereo el where
  getChannel (Stereo a _) 0 = a
  getChannel (Stereo _ b) 1 = b
  getChannel _          _ = error "getChannel Stereo invalid index"
  {-# INLINE getChannel #-}

  setChannel 0 a (Stereo _ b) = Stereo a b
  setChannel 1 b (Stereo a _) = Stereo a b
  setChannel _ _ _          = error "setChannel Stereo invalid index"
  {-# INLINE setChannel #-}

  mapChannel f 0 (Stereo a b) = Stereo (f a) b
  mapChannel f 1 (Stereo a b) = Stereo a     (f b)
  mapChannel _ _ _          = error "setChannel Stereo invalid index"
  {-# INLINE mapChannel #-}

  toC (a:b:_) = Stereo a b
  toC _       = error "Stereo toC invalid input list"
  {-# INLINE toC #-}

  fromC (Stereo a b) = [a,b]
  {-# INLINE fromC #-}

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
  setChannel _ _ _        = error "setChannel Mono invalid index"
  {-# INLINE setChannel #-}

  mapChannel f 0 (Mono a) = Mono (f a)
  mapChannel _ n _        = error $ "mapChannel Mono invalid index " ++ show n
  {-# INLINE mapChannel #-}

  toC (a:_) = Mono a
  toC _     = error "toC Mono invalid input list"

  fromC (Mono a) = [a]

-- -------------------------------------------------------
-- Support for channelized vectors

-- |A StorableVector with a type-level channel definition.
data ChannelizedVector s a = CVec !(V a)

-- |A value-level representation of the number of channels in the vector.
-- O1
numChannels :: forall s a. (T.Nat s) => ChannelizedVector s a -> NumChannels
numChannels _ = T.toNum (undefined :: s)

-- |Create an interleaved vector from a ChannelizedVector.
-- O1
interleave :: ChannelizedVector s a -> V a
interleave (CVec v) = v

-- |Create a ChannelizedVector.  The vector length
-- must be a multiple of the number of channels requested.
-- In CPS style because of the data reification.
channelize :: NumChannels
  -> V a
  -> (forall s. (T.Nat s) =>
        Maybe (ChannelizedVector s a)
        -> r)
  -> r
channelize n v k = T.reifyIntegral n (\s -> k $ channelizeT s v)

-- |Create a ChannelizedVector.  The vector length is determined
-- by the type of `s'.
channelizeT :: forall s a. (T.Nat s) =>
  s
  -> V a
  -> Maybe (ChannelizedVector s a)
channelizeT _ v = if SV.length v `rem` nc == 0 && nc > 0
  then Just (CVec v)
  else Nothing
  where
    nc = T.toInt (undefined :: s)

-- |Make a copy of the specified channel from a ChannelizedVector.
getChannelV :: (T.Nat s, Storable a) => ChannelizedVector s a
  -> ChannelIndex
  -> V a
getChannelV cv n = SV.sample (fI $ numFrames cv) (\i -> index cv (fI i) n)

-- |Return a frame of data
getFrame :: (T.Nat s, Storable a) =>
  ChannelizedVector s a
  -> FrameCount
  -> [a]
getFrame cv fc = map (index cv fc) [0 .. nc - 1]
  where
    nc = fI $ numChannels cv

rawPosition :: NumChannels -> FrameCount -> ChannelIndex -> Int
rawPosition nc fc c = (fI nc * fI fc) + (fI c)

-- |Calculate the number of frames in a ChannelizedVector
numFrames :: T.Nat s => ChannelizedVector s a -> FrameCount
numFrames cv@(CVec v) = fI (SV.length v) `div` nc
  where
    nc = numChannels cv

-- |Look up a value in a ChannelizedVector by frame and channel.
index :: (T.Nat s, Storable a) =>
  ChannelizedVector s a
  -> FrameCount
  -> ChannelIndex
  -> a
index cv@(CVec v) fc c = SV.index v (rawPosition nc fc c)
  where
    nc = numChannels cv

-- --------------------------------------------
-- Basic interface

-- |Take n frames from a ChannelizedVector.
take :: (T.Nat s, Storable a) =>
  ChannelizedVector s a
  -> FrameCount
  -> ChannelizedVector s a
take cv@(CVec v) fc = CVec $ SV.take (nc * fI fc) v
  where nc = fI $ numChannels cv

-- |Drop n frames from a ChannelizedVector.
drop :: (T.Nat s, Storable a) => ChannelizedVector s a
  -> FrameCount
  -> ChannelizedVector s a
drop cv@(CVec v) fc = CVec $ SV.drop (fI nc * fI fc) v
  where nc = numChannels cv

-- |Split a ChannelizedVector into two at the given frame position.
splitAt :: (T.Nat s, Storable a) => ChannelizedVector s a
  -> FrameCount
  -> (ChannelizedVector s a, ChannelizedVector s a)
splitAt cv@(CVec v) fc = (CVec v1, CVec v2)
  where
    (v1, v2) = SV.splitAt (fI nc * fI fc) v
    nc = numChannels cv

-- --------------------------------------------
-- Maps

-- |Map a function uniformly over all channels.
-- O(n)
mapAll :: (Storable a) => (a -> a)
  -> ChannelizedVector s a
  -> ChannelizedVector s a
mapAll f (CVec v) = CVec $ SV.map f v

-- |Map a list of functions, one per channel, over a ChannelizedVector.
-- O(n)
mapChannels :: (Channelized s c (a -> a), Storable a) => c s (a -> a)
  -> ChannelizedVector s a
  -> ChannelizedVector s a
mapChannels fs cv@(CVec v) = CVec $ SV.mapIndexed f v
  where
    nc = numChannels cv
    f i = fs `getChannel` (normChn nc i)

{- --I'd like to test this version, but mapIndexed may be a better choice.
-- currently this code is out of date
mapChannels fs cv@(CVec nc v) = CVec nc $
  SV.sample (fI $ numFrames cv) (\i ->
    f (normChn nc i) (SV.index v i))
  where
    f n = fs !! n
-}

-- |Map a function over one channel of a ChannelizedVector.
-- O(n), where n is the total length of the ChannelizedVector.
mapChannelV :: (T.Nat s, Storable a) => (a -> a)
  -> ChannelIndex
  -> ChannelizedVector s a
  -> ChannelizedVector s a
mapChannelV f n cv@(CVec v) = CVec $ SV.mapIndexed f' v
  where
    nc = numChannels cv
    f' i = if (normChn nc i) == fI n then f else id

-- ----------------
-- folds
-- I'm not doing many folds yet, we'll see what I need.

-- |Perform a fold over each channel.  This should be essentially the same as
-- foldl' below, once I've got a solid implementation for it.
foldl :: (Channelized s c (a -> b -> a),
    Channelized s c a,
    Channelized s c (SV.Vector b),
    Storable b) =>
  c s (a -> b -> a)
  -> c s a
  -> ChannelizedVector s b
  -> c s a
foldl fs i0s cv@(CVec v0) = hopfoldl <$> fs <*> i0s <*> pure (fI nc) <*>
                             toC (map (flip SV.drop v0) [0 .. (fI $ nc-1)])
  where
    nc = numChannels cv
{-# INLINE foldl #-}

foldl' :: (Channelized s c (a -> b -> a),
    Channelized s c a,
    Channelized s c (SV.Vector b),
    Storable b) =>
  c s (a -> b -> a)
  -> c s a
  -> ChannelizedVector s b
  -> c s a
foldl' fs i0s cv@(CVec v0) = hopfoldl' <$> fs <*> i0s <*> pure (fI nc) <*>
                             toC (map (flip SV.drop v0) [0 .. (fI $ nc-1)])
  where
    nc = numChannels cv
{-# INLINE foldl' #-}

-- Helper function for calculating channel indices
normChn :: NumChannels -> ChannelIndex -> ChannelIndex
normChn 1 _  = 0
normChn nc i = i `rem` (fI nc)

hopfoldl :: (Storable a) => (b -> a -> b) -> b -> Int -> SV.Vector a -> b
hopfoldl f v hop (SVB.SV x s l) =
   unsafePerformIO $ withForeignPtr x $ \ptr ->
      let sptr = ptr `advancePtr` s
          go pos p z = if pos >= l
                     then return z
                     else (Foreign.Storable.peek p >>= go (pos + hop)
                          (p `advancePtr` hop) . f z)
      in  go 0 sptr v
{-# INLINE hopfoldl #-}

hopfoldl' :: (Storable a) => (b -> a -> b) -> b -> Int -> SV.Vector a -> b
hopfoldl' f v hop (SVB.SV x s l) =
   unsafePerformIO $ withForeignPtr x $ \ptr ->
      let sptr = ptr `advancePtr` s
          go pos p z = if pos >= l
                     then return z
                     else z `seq` (Foreign.Storable.peek p >>= go (pos + hop)
                          (p `advancePtr` hop) . f z)
      in  go 0 sptr v
{-# INLINE hopfoldl' #-}

-- ----------------------------------------
-- Enumerator support.

-- | An enumerator that creates an interleaved stream from a channelized
-- stream.
interleaveStream :: (T.Nat s, Monad m) =>
  EnumeratorN [] (ChannelizedVector s Double) V Double m a
interleaveStream = I.convStream muxFunc

-- |An Iteratee to be used in convStream to mux a channelized stream.
muxFunc :: (T.Nat s, Monad m) =>
           IterateeG [] (ChannelizedVector s Double) m
                        (Maybe (V Double))
muxFunc = IterateeG step
  where
  step (Chunk [])     = return $ Cont (IterateeG step) Nothing
  step (Chunk (v:vs)) = return $ Done (Just $ interleave v) (Chunk vs)
  step str            = return $ Done Nothing str

-- | A stream enumerator to convert an interleaved audio stream to a 
-- channelized stream.
channelizeStream :: (T.Nat s, Monad m) =>
  s
  -> EnumeratorN V Double [] (ChannelizedVector s Double) m a
channelizeStream = I.convStream . deMuxFunc

-- | An iteratee to convert an interleaved stream to a channelized stream.
deMuxFunc :: (T.Nat s, Monad m) =>
             s ->
             IterateeG V Double m (Maybe ([ChannelizedVector s Double]))
deMuxFunc n = IterateeG step
  where
  n' = T.toInt n
  step c@(Chunk v) | SV.length v < n' = return $ Cont (step' c) Nothing
  step (Chunk v) = let (vecs, rm) = channelizeVector n v in
                   return $ Done (fmap (:[]) vecs) $ Chunk rm
  step str = return $ Done Nothing str
  step' i0 = IterateeG (step . mappend i0)

-- | Split an interleaved vector to a list of channelized vectors.
-- The second half of the tuple is any data remaining after splitting
-- to channels.
channelizeVector :: (T.Nat s) =>
  s
  -> V Double
  -> (Maybe (ChannelizedVector s Double), V Double)
channelizeVector n v = first (channelizeT n) $ SV.splitAt chanLen v
  where
    n' = T.toInt n
    chanLen = SV.length v - (SV.length v `rem` n')
