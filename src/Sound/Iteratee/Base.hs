module Sound.Iteratee.Base (
  AudioStreamState (..),
  AudioStack
)

where

import Control.Monad.State

-- |Information about the AudioStream
data AudioStreamState =
     WaveState  Integer Integer -- ^ Total bytes written, data chunklen offset
     | NoState

-- | Audio monad stack (for writing files)
type AudioStack = StateT AudioStreamState IO
