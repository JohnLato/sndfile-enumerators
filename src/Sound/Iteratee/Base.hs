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
  NumChannels,
  SampleRate,
  BitDepth
)

where

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
