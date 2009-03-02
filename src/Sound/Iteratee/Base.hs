module Sound.Iteratee.Base (
  AudioStreamState (..),
  WritableAudio (..),
  AudioMonad
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

