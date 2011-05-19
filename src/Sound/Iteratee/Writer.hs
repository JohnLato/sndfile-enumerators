{-# LANGUAGE RankNTypes, FlexibleContexts #-}

{- | Generic functions and iteratees to support writing files.
-}

module Sound.Iteratee.Writer (
  -- * Audio writing functions
  fileDriverAudio
  ,runAudioMonad
  ,enumAudioFile
)

where

import Sound.Iteratee.Base
import Sound.Iteratee.Codecs

import Data.Iteratee
import Data.Iteratee.IO
import qualified Data.Vector.Storable as V
import Data.Word (Word8)
import Control.Monad.CatchIO

runAudioMonad :: AudioMonad a -> IO a
runAudioMonad am = do
  (a, s) <- runStateT am NoState
  case s of
    NoState     -> return a
    WaveState{} -> runWaveAM (put s >> return a)

-- | A simplified interface to running an audio iteratee
fileDriverAudio ::
  Iteratee (V.Vector Word8) AudioMonad a
  -> FilePath
  -> IO a
fileDriverAudio i fp = runAM (fileDriverRandom i fp)
  where
    runAM = runAudioMonad

enumAudioFile ::
  MonadCatchIO m
  => Int
  -> FilePath
  -> Enumerator (V.Vector Word8) m a
enumAudioFile = enumFileRandom
