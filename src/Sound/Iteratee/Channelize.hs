module Sound.Iteratee.Channelize (
  -- * Functions
  -- ** Multichannel support functions
  interleaveStream
  ,channelizeStream
)

where

import Prelude as P
import Sound.Iteratee.Instances()
import qualified Sound.Iteratee.ChannelizedVector as CV
import Sound.Iteratee.ChannelizedVector (ChannelizedVector)

import Data.Iteratee
import Data.Monoid
import qualified Data.StorableVector as SV
import qualified Data.StorableVector.Base as SVB

import Control.Arrow
import qualified Data.TypeLevel.Num as T

-- internal type synonym
type V = SV.Vector

-- | An enumerator that creates an interleaved stream from a channelized
-- stream.
interleaveStream :: (T.Nat s, Monad m) =>
  EnumeratorN [] (ChannelizedVector s Double) V Double m a
interleaveStream = convStream muxFunc

-- |An Iteratee to be used in convStream to mux a channelized stream.
muxFunc :: (T.Nat s, Monad m) =>
           IterateeG [] (ChannelizedVector s Double) m
                        (Maybe (V Double))
muxFunc = IterateeG step
  where
  step (Chunk [])     = return $ Cont (IterateeG step) Nothing
  step (Chunk (v:vs)) = return $ Done (Just $ CV.interleave v) (Chunk vs)
  step str            = return $ Done Nothing str

-- | A stream enumerator to convert an interleaved audio stream to a 
-- channelized stream.
channelizeStream :: (T.Nat s, Monad m) =>
  s
  -> EnumeratorN V Double [] (ChannelizedVector s Double) m a
channelizeStream = convStream . deMuxFunc

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
channelizeVector n v = first (CV.channelizeT n) $ SV.splitAt chanLen v
  where
    n' = T.toInt n
    chanLen = SV.length v - (SV.length v `rem` n')
