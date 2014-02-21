{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -Wall -fsimpl-tick-factor=200 #-}
module Sound.Iteratee.File (
  getFormat
 ,getAudioInfo
 ,defaultBufSize

 ,genAudio
 ,runAudioConsumer
)

where

import           Sound.Iteratee.Base
import           Sound.Iteratee.Codecs
import           Sound.Iteratee.Codecs.Wave ()
import           Sound.Iteratee.Writer
import           IterX
import           IterX.Fusion

import           System.FilePath
import           Data.Char

-- | Default buffer size.  The value from Data.Iteratee.IO is generally too
-- small for good performance.
defaultBufSize :: Int
defaultBufSize = 2 ^ (16 :: Int)

-- | get the format from a file name
getFormat :: FilePath -> Maybe SupportedFileFormat
getFormat fp = case ext of
 "wav"  -> Just Wave
 "wave" -> Just Wave
 _      -> Nothing
 where
  ext = map toLower . tail $ takeExtension fp -- drop the initial "."

-- | get audio format information and audio length (samples, not frames)
-- from a file
getAudioInfo :: FilePath -> IO (Maybe (AudioFormat, Integer))
getAudioInfo fp = case getFormat fp of
  Just Wave -> error "getAudioInfo: TODO"  -- fileDriverAudio (simpleDictSoundInfo <$> waveReadDict) fp
  Just Raw  -> return Nothing
  _         -> return Nothing -- could try everything and see what matches...
{-# INLINE getAudioInfo #-}

genAudio :: (ExIO m, Functor m)
         => FilePath -> FoldM m NormFormattedChunk o -> m o
genAudio fp ofold = case getFormat fp of
    Just Wave -> runFold (rawToWaveTrans ofold)
                  $ yieldFileChunks fp defaultBufSize
    Just Raw  -> error "genAudio: Raw format not implemented"
    Nothing   -> error $ "genAudio: couldn't determine file format: " ++ show fp
{-# INLINE [1] genAudio #-}

runAudioConsumer ::
  FilePath
  -> Consumer AudioMonad NormFormattedChunk
  -> IO ()
runAudioConsumer fp c = runAudioMonad $ genAudio fp
                          $ foldingM (\_ o -> c o) ()
