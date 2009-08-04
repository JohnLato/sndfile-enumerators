module Sound.Iteratee.Base (
  -- * Types
  -- ** Internal types
  AudioStreamState (..),
  WritableAudio (..),
  AudioMonad,
  -- *** Functions to work with AudioMonad
  module Control.Monad.Trans.State,
  -- ** Audio Format types
  AudioFormat (..),
  SupportedBitDepths (..),
  NumChannels,
  SampleRate,
  BitDepth,
  FrameCount,
  -- ** Multichannel support functions
  mux,
  deMux
)

where

import Prelude as P
import Sound.Iteratee.Instances()

import Data.Iteratee
import Data.List
import Data.Monoid
import qualified Data.StorableVector as SV
import qualified Data.StorableVector.Base as SVB
import Foreign.Marshal.Array
import Foreign.Ptr
import Control.Monad.Trans.State
import Control.Parallel.Strategies
import System.IO

-- |Information about the AudioStream
data AudioStreamState =
  WaveState !(Maybe Handle) !(Maybe AudioFormat) !Integer !Integer !Integer -- ^ Handle, format, Total bytes written, data bytes written, data chunklen offset
  | NoState
  deriving (Eq, Show)

class WritableAudio a where
  emptyState :: a -> AudioStreamState
  initState ::  a -> Handle -> AudioStreamState
  supportedBitDepths :: a -> SupportedBitDepths

-- | Audio monad stack (for writing files)
type AudioMonad = StateT AudioStreamState IO

-- | Format of audio data
data AudioFormat = AudioFormat {
  numberOfChannels :: NumChannels, -- ^Number of channels in the audio data
  sampleRate :: SampleRate, -- ^Sample rate of the audio data
  bitDepth :: BitDepth -- ^Bit depth of the audio data
  } deriving (Show, Eq)

instance NFData AudioFormat where
  rnf (AudioFormat nc sr bd) = rnf nc >| rnf sr >| rnf bd

type NumChannels = Integer
type SampleRate  = Integer
type BitDepth    = Integer
type FrameCount  = Integer

data SupportedBitDepths = Any | Supported [BitDepth]

-- -------------------------
-- Multichannel support functions

-- internal type synonym
type V = SV.Vector

-- | An enumerator that creates an interleaved stream from a channelized
-- stream.
mux :: Monad m => AudioFormat -> EnumeratorN [] (V Double) V Double m a
mux = convStream . muxFunc . numberOfChannels

-- |An Iteratee to be used in convStream to mux a channelized stream.
muxFunc :: Monad m =>
           NumChannels ->
           IterateeG [] (V Double) m (Maybe (V Double))
muxFunc n = IterateeG step
  where
  nInt = fromIntegral n
  step (Chunk [])   = return $ Cont (muxFunc n) Nothing
  step (Chunk vs) | P.length vs >= nInt = let (hs, r) = splitAt nInt vs in
                      return $ Done (Just $ interleaveVectors hs) (Chunk r)
  step c@(Chunk _)  = return $ Cont (step' c) Nothing
  step str          = return $ Done Nothing str
  step' i0          = IterateeG (step . mappend i0)

-- | Interleave a list of vectors (channel streams) into one stream.
interleaveVectors :: [V Double] -> V Double
interleaveVectors vs = SVB.unsafeCreate (sum $ fmap SV.length vs)
                       (muxCreateV vs)

-- | Create a buffer by interleaving a list of vectors.
-- To be used with SVB.unsafeCreate.
-- This function will fail if the vectors are of different lengths.
muxCreateV :: [V Double] -> Ptr Double -> IO ()
muxCreateV [] _   = return ()
muxCreateV vs ptr = case mlen of
  Just 0  -> return ()
  Just _n -> pokeArray ptr $ concat . transpose . fmap SV.unpack $ vs
  Nothing -> error "Stream error: unequal channel stream lengths"
  where
  pLen = SV.length $ P.head vs
  mlen = case all (== pLen) (fmap SV.length $tail vs) of
    True -> Just pLen
    False -> Nothing

-- | A stream enumerator to convert an interleaved audio stream to a 
-- channelized stream.
deMux :: Monad m => AudioFormat -> EnumeratorN V Double [] (V Double) m a
deMux = convStream . deMuxFunc . numberOfChannels

-- | An iteratee to convert an interleaved stream to a channelized stream.
deMuxFunc :: Monad m =>
             NumChannels ->
             IterateeG V Double m (Maybe ([V Double]))
deMuxFunc n = IterateeG step
  where
  n' = fromIntegral n
  step c@(Chunk v) | SV.length v < n' = return $ Cont (step' c) Nothing
  step (Chunk v) = let (vecs, rm) = splitVector n v in
                   return $ Done (Just vecs) $ Chunk rm
  step str = return $ Done Nothing str
  step' i0 = IterateeG (step . mappend i0)

-- | Split an interleaved vector to a list of channelized vectors.
-- The second half of the tuple is any data remaining after splitting
-- to channels.
splitVector :: NumChannels -> V Double -> ([V Double], V Double)
splitVector 1 vec = ([vec], SV.empty)
splitVector _n vec | SV.null vec = ([], SV.empty)
splitVector n _vec | n <= 0 = error $ "Cannot demux " ++ show n ++ " channels."
splitVector n vec = (channels, rm)
  where
  nInt = fromIntegral n
  (first, rm) = SV.splitAt (SV.length vec - (SV.length vec `mod` nInt)) vec
  channels = [SVB.unsafeCreate (SV.length first `div` nInt)
               (demuxCreateV a nInt vec) | a <- [0 .. nInt - 1]]

-- This function is meant to be used with Data.StorableVector.create
-- the first parameter is the offset from the front of the vector, and the
-- second is the hop size (e.g. 2 for 2-channel, etc.)
demuxCreateV :: Int -> Int -> V Double -> Ptr Double -> IO ()
demuxCreateV off hop vec ptr =
  pokeArray ptr [SV.index vec (off + (x * hop)) | x <- [0 .. rm]]
  where
  rm = (SV.length vec `div` hop) - 1
