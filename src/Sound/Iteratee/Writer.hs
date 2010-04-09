{-# LANGUAGE RankNTypes, FlexibleContexts #-}

{- | Generic functions and iteratees to support writing files.
-}

module Sound.Iteratee.Writer (
  -- * Audio writing functions
  fileDriverAudio,
  runAudioMonad
)

where

import Sound.Iteratee.Base
import Sound.Iteratee.Codecs

import Data.MutableIter
import Data.MutableIter.IOBuffer (IOBuffer)

import Control.Monad.Trans.Region
import Foreign.Storable (Storable)

runAudioMonad :: AudioMonad a -> IO a
runAudioMonad am = do
  (a, s) <- runStateT am NoState
  case s of
    NoState     -> return a
    WaveState{} -> runWaveAM (put s >> return a)

fileDriverAudio :: (Storable el) =>
  (forall s.  MIteratee (IOBuffer (RegionT s AudioMonad) el)
                        (RegionT s AudioMonad)
                        a)
  -> FilePath
  -> IO a
fileDriverAudio i fp = runAM (fileDriverRandom defaultChunkLength i fp)
  where
    runAM = runAudioMonad
