module Sound.Iteratee.ChannelizedVector (
  ChannelizedVector
  ,numChannels
  ,interleave
  ,channelize
  ,numFrames
  ,index
  ,getChannel
  ,getFrame
  ,take
  ,drop
  ,splitAt
  ,mapAll
  ,mapChannels
  ,mapChannel
  ,foldl
  ,foldl'
)

where

import Prelude hiding (take, drop, splitAt, foldl)

import Sound.Iteratee.Base
import qualified Data.StorableVector as SV
import Foreign.Storable

import Control.Arrow
import Control.Applicative
import Control.Parallel.Strategies

type V = SV.Vector

type ChannelIndex = Int

-- helper function
fI :: (Integral a, Num b) => a -> b
fI = fromIntegral

-- |Create a channelized vector from an interleaved vector
data ChannelizedVector a = CVec !NumChannels !(V a)

numChannels :: ChannelizedVector a -> NumChannels
numChannels (CVec nc _) = nc

interleave :: ChannelizedVector a -> V a
interleave (CVec _ v) = v

-- |Create a ChannelizedVector.  The vector length
-- must be a multiple of the number of channels requested.
channelize :: NumChannels -> V a -> Maybe (ChannelizedVector a)
channelize n v = if (SV.length v `rem` (fI n) == 0) && (n > 0)
  then Just (CVec n v)
  else Nothing

-- |Make a copy of the specified channel from a ChannelizedVector.
getChannel :: Storable a => ChannelizedVector a -> ChannelIndex -> V a
getChannel cv n = SV.sample (fI $ numFrames cv) (\i -> index cv (fI i) n)

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
mapChannels :: Storable a => [(a -> a)]
  -> ChannelizedVector a
  -> ChannelizedVector a
mapChannels fs (CVec nc v) = CVec nc $ SV.mapIndexed f v
  where
    f i = fs !! (i `rem` (fI nc))

{- --I'd like to test this version, but mapIndexed may be a better choice.
mapChannels fs cv@(CVec n v) = CVec n $
  SV.sample (fI $ numFrames cv) (\i ->
    f (i `rem` (fI n)) (SV.index v i))
  where
    f n = fs !! n
-}

-- |Map a function over one channel of a ChannelizedVector.
-- O(n), where n is the total length of the ChannelizedVector.
mapChannel :: Storable a => (a -> a)
  -> ChannelIndex
  -> ChannelizedVector a
  -> ChannelizedVector a
mapChannel f n (CVec nc v) = CVec nc $ SV.mapIndexed f' v
  where
    f' i = if (i `rem` (fI nc)) == fI n then f else id

-- ----------------
-- folds
-- I'm not doing many folds yet, we'll see what I need.

-- |Perform a fold over each channel
foldl :: Storable a => [(b -> a -> b)] -> [b] -> ChannelizedVector a -> [b]
foldl fs i0s (CVec nc v0) = go (ZipList i0s) v0
  where
    go acc v | SV.null v = getZipList acc
    go acc v = let (this, next) = SV.splitAt (fI nc) v
                   curr = ZipList fs <*> acc <*> (ZipList $ SV.unpack this)
               in go curr next

-- In addition to the applicative version, I could also try a mapAccum
-- implementation, or a lower-level version.
-- This isn't good.  Try the StorableVector fold where the function rotates
-- through the functions and accumulators.

foldl' :: (NFData b, Storable a) => [(b -> a -> b)]
  -> [b]
  -> ChannelizedVector a
  -> [b]
foldl' fs i0s (CVec nc v0) = go (ZipList i0s) v0
  where
    go acc v | SV.null v = getZipList acc
    go acc v = let (this, next) = SV.splitAt (fI nc) v
                   curr = (ZipList fs <*> acc <*> (ZipList $ SV.unpack this))
                          `using` rnf
               in curr `seq` go curr next

-- |We need an NFData instance for ZipList.
instance NFData b => NFData (ZipList b) where
  rnf = rnf . getZipList
