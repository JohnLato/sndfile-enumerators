module Sound.Iteratee.Channelize (
  -- * Functions
  -- ** Multichannel support functions
  mux,
  deMux
)

where

import Prelude as P
import Sound.Iteratee.Instances()
import Sound.Iteratee.Base
import qualified Sound.Iteratee.ChannelizedVector as CV
import Sound.Iteratee.ChannelizedVector (ChannelizedVector)

import Data.Iteratee
import Data.Monoid
import qualified Data.StorableVector as SV
import qualified Data.StorableVector.Base as SVB

import Control.Arrow

-- internal type synonym
type V = SV.Vector

-- | An enumerator that creates an interleaved stream from a channelized
-- stream.
mux :: Monad m =>  EnumeratorN [] (ChannelizedVector Double) V Double m a
mux = convStream muxFunc

-- |An Iteratee to be used in convStream to mux a channelized stream.
muxFunc :: Monad m =>
           IterateeG [] (ChannelizedVector Double) m
                        (Maybe (V Double))
muxFunc = IterateeG step
  where
  step (Chunk [])     = return $ Cont (IterateeG step) Nothing
  step (Chunk (v:vs)) = return $ Done (Just $ CV.interleave v) (Chunk vs)
  step str            = return $ Done Nothing str

-- | A stream enumerator to convert an interleaved audio stream to a 
-- channelized stream.
deMux :: Monad m => AudioFormat -> EnumeratorN V Double [] (ChannelizedVector Double) m a
deMux = convStream . deMuxFunc . numberOfChannels

-- | An iteratee to convert an interleaved stream to a channelized stream.
deMuxFunc :: Monad m =>
             NumChannels ->
             IterateeG V Double m (Maybe ([ChannelizedVector Double]))
deMuxFunc n = IterateeG step
  where
  n' = fromIntegral n
  step c@(Chunk v) | SV.length v < n' = return $ Cont (step' c) Nothing
  step (Chunk v) = let (vecs, rm) = channelizeVector n v in
                   return $ Done (fmap (:[]) vecs) $ Chunk rm
  step str = return $ Done Nothing str
  step' i0 = IterateeG (step . mappend i0)

-- | Split an interleaved vector to a list of channelized vectors.
-- The second half of the tuple is any data remaining after splitting
-- to channels.
channelizeVector :: NumChannels
  -> V Double
  -> (Maybe (ChannelizedVector Double), V Double)
channelizeVector n v = first (CV.channelize n) $ SV.splitAt chanLen v
  where
    n' = fromIntegral n
    chanLen = SV.length v - (SV.length v `rem` n')
