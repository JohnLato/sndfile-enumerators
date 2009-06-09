{- | Generic functions and iteratees to support writing files.
-}

module Sound.Iteratee.Writer (
  -- * Audio writing functions
  fileDriverAudio,
  runAudioMonad
)

where

import Sound.Iteratee.Base
import Sound.Iteratee.IO
import Sound.Iteratee.Codecs
import Data.Iteratee
import Data.Iteratee.Base.StreamChunk

import System.IO

-- |Process a file using the given IterateeG.  This function wraps
-- enumAudioFile as a convenience.

runAudioMonad :: AudioMonad a -> IO a
runAudioMonad am = do
  (a, s) <- runStateT am NoState
  case s of
    NoState     -> return a
    WaveState{} -> runWaveAM (put s >> return a)

fileDriverAudio :: ReadableChunk s el => IterateeG s el AudioMonad a ->
               FilePath ->
               IO a
fileDriverAudio iter filepath = do
  h <- openBinaryFile filepath ReadMode
  result <- runAudioMonad (enumAudioFile h iter >>= run)
  hClose h
  return result

