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
  -- ** File Format Types
  SupportedFileFormat (..),
  -- * Functions
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
import Control.Monad.Trans.State
import Control.Parallel.Strategies
import System.IO

-- |Information about the AudioStream
data AudioStreamState =
  WaveState !(Maybe Handle) !(Maybe AudioFormat) !Integer !Integer !Integer -- ^ Handle, format, Total bytes written, data bytes written, data chunklen offset
  | NoState
  deriving (Eq, Show)

-- | An enumeration of all file types supported for reading and writing.
data SupportedFileFormat = Raw
                           | Wave
                           deriving (Show, Enum, Bounded, Eq)

-- | Common functions for writing audio data
class WritableAudio a where
  emptyState :: a -> AudioStreamState
  initState ::  a -> Handle -> AudioStreamState
  supportedBitDepths :: a -> SupportedBitDepths
  fileType           :: a -> SupportedFileFormat

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
muxFunc 1 = noIter -- we can take a shortcut for mono functions
  where
  noIter = IterateeG step
  step (Chunk [])     = return $ Cont noIter Nothing
  -- the tail should always be the empty list here, but it's handled the
  -- same in either case so no need to verify.
  step (Chunk (v:vs)) = return $ Done (Just $ v) (Chunk vs)
  step str        = return $ Done Nothing str
muxFunc n = IterateeG step
  where
  nInt = fromIntegral n
  step (Chunk [])   = return $ Cont (IterateeG step) Nothing
  step (Chunk vs) | P.length vs >= nInt = let (hs, r) = splitAt nInt vs in
                      return $ Done (Just $ interleaveVectors hs) (Chunk r)
  step c@(Chunk _)  = return $ Cont (step' c) Nothing
  step str          = return $ Done Nothing str
  step' i0          = IterateeG (step . mappend i0)

-- | Interleave a list of vectors (channel streams) into one stream.
interleaveVectors :: [V Double] -> V Double
interleaveVectors = SV.pack . concat . transpose . map SV.unpack

-- | A stream enumerator to convert an interleaved audio stream to a 
-- channelized stream.
deMux :: Monad m => AudioFormat -> EnumeratorN V Double [] (V Double) m a
deMux = convStream . deMuxFunc . numberOfChannels

-- | An iteratee to convert an interleaved stream to a channelized stream.
deMuxFunc :: Monad m =>
             NumChannels ->
             IterateeG V Double m (Maybe ([V Double]))
deMuxFunc 1 = noIter
  where
  noIter = IterateeG step
  step (Chunk vs) | SV.null vs = return $ Cont noIter Nothing
  step (Chunk vs) = return $ Done (Just [vs]) (Chunk mempty)
  step str        = return $ Done Nothing str
deMuxFunc n = IterateeG step
  where
  n' = fromIntegral n
  step c@(Chunk v) | SV.length v < n' = return $ Cont (step' c) Nothing
  step (Chunk v) = let (vecs, rm) = channelizeVector n v in
                   return $ Done (Just vecs) $ Chunk rm
  step str = return $ Done Nothing str
  step' i0 = IterateeG (step . mappend i0)

-- | Split an interleaved vector to a list of channelized vectors.
-- The second half of the tuple is any data remaining after splitting
-- to channels.
channelizeVector :: NumChannels -> V Double -> ([V Double], V Double)
channelizeVector n v = (splits, rm)
  where
  rm = SV.drop (fromIntegral n * pLen) v
  pLen = SV.length v `div` fromIntegral n
  hop = fromIntegral $ n - 1
  splits = map (fst . SV.unfoldrN pLen unfolder . flip SV.drop v) [0 .. hop]
  unfolder = fmap (\(a, b) -> (a, SV.drop hop b)) . SV.viewL
