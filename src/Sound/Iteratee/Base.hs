module Sound.Iteratee.Base (
  -- * Types
  -- ** Internal types
  AudioStreamState (..),
  WritableAudio (..),
  AudioMonad,
  -- ** Audio Format types
  AudioFormat (..),
  NumChannels,
  SampleRate,
  BitDepth
)

where

import Control.Monad.State

-- |Information about the AudioStream
data AudioStreamState =
  WaveState  Integer Integer -- ^ Total bytes written, data chunklen offset
  | NoState

class WritableAudio a where
  emptyState :: a -> AudioStreamState

-- | Audio monad stack (for writing files)
type AudioMonad = StateT AudioStreamState IO

-- | Format of audio data
data AudioFormat = AudioFormat {
  numberOfChannels :: NumChannels, -- ^Number of channels in the audio data
  sampleRate :: SampleRate, -- ^Sample rate of the audio data
  bitDepth :: BitDepth -- ^Bit depth of the audio data
  } deriving (Show, Eq)

type NumChannels = Integer
type SampleRate  = Integer
type BitDepth    = Integer
