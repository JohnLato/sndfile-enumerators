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

import Data.Iteratee.Base.ReadableChunk
import Data.Iteratee.IO
import Data.MutableIter

import Control.Monad.Trans.Region

runAudioMonad :: AudioMonad a -> IO a
runAudioMonad am = do
  (a, s) <- runStateT am NoState
  case s of
    NoState     -> return a
    WaveState{} -> runWaveAM (put s >> return a)

fileDriverAudio :: forall a el st. (ReadableChunk (st el) el) =>
  (forall s.  MIteratee (st el) (RegionT s AudioMonad) a)
  -> FilePath
  -> IO a
fileDriverAudio i fp = runAM (runRegionT (fileDriverRandom (unwrap i) fp))
  where
    runAM = runAudioMonad
