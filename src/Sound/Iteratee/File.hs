module Sound.Iteratee.File (
  getFormat
 ,getAudioInfo
)

where

import           Sound.Iteratee.Base
import           Sound.Iteratee.Codecs
import           Sound.Iteratee.Writer

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
  _         -> return Nothing
