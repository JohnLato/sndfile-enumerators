{-# LANGUAGE RankNTypes #-}

module Sound.Iteratee.File (
  getFormat
 ,getAudioInfo
 ,runAudioIteratee
)

where

import           Sound.Iteratee.Base
import           Sound.Iteratee.Codecs
import           Sound.Iteratee.Writer
import           Data.Iteratee
import qualified Data.Vector.Storable as V

import           System.FilePath
import           Data.Char

-- | get the format from a file name
getFormat :: FilePath -> Maybe SupportedFileFormat
getFormat fp = case ext of
 "wav"  -> Just Wave
 "wave" -> Just Wave
 _      -> Nothing
 where
  ext = map toLower . tail $ takeExtension fp -- drop the initial "."

-- | get audio format information from a file
getAudioInfo :: FilePath -> IO (Maybe AudioFormat)
getAudioInfo fp = case getFormat fp of
  Just Wave -> fileDriverAudio (waveReader >>=
             maybe (return Nothing) dictReadFirstFormat) fp
  Just Raw  -> return Nothing
  _         -> return Nothing -- could try everything and see what matches...

runAudioIteratee ::
  FilePath
  -> (Iteratee (V.Vector Double) AudioMonad a)
  -> IO (Maybe a)
runAudioIteratee fp iter = case getFormat fp of
  Just Wave -> fileDriverAudio (waveReader >>= \(Just dict) ->
                    dictProcessData_ 0 dict iter) fp
  Just Raw  -> return Nothing
  _         -> return Nothing
