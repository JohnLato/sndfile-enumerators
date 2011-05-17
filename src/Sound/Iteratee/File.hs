{-# LANGUAGE RankNTypes #-}

module Sound.Iteratee.File (
  getFormat
 ,getAudioInfo
 ,runAudioIteratee
 ,enumAudioIteratee
 ,enumAudioIterateeWithFormat
 ,defaultBufSize
)

where

import           Sound.Iteratee.Base
import           Sound.Iteratee.Codecs
import           Sound.Iteratee.Codecs.Wave ()
import           Sound.Iteratee.Writer
import           Data.Iteratee
import           Data.Iteratee.IO (defaultBufSize)
import qualified Data.Vector.Storable as V

import           Control.Exception
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

enumAudioIterateeWithFormat ::
  FilePath
  -> (AudioFormat -> Iteratee (V.Vector Double) AudioMonad a)
  -> AudioMonad (Iteratee (V.Vector Double) AudioMonad a)
enumAudioIterateeWithFormat fp fi = case getFormat fp of
  Just Wave -> run =<< enumAudioFile defaultBufSize fp (waveReader >>= wFn)
  Just Raw  -> return . throwErr $ iterStrExc "Raw format not yet implemented"
  _         -> return . throwErr $ iterStrExc "Raw format not yet implemented"
 where
  wFn = maybe (throwErr $ toException CorruptFileException)
              (\d -> do
                mFmt <- dictReadFirstFormat d
                maybe (throwErr $ toException MissingFormatException)
                      (dictProcessData 0 d . fi) mFmt )

enumAudioIteratee ::
  FilePath
  -> Iteratee (V.Vector Double) AudioMonad a
  -> AudioMonad (Iteratee (V.Vector Double) AudioMonad a)
enumAudioIteratee fp i = enumAudioIterateeWithFormat fp (const i)

runAudioIteratee :: Exception e
  => FilePath
  -> Iteratee (V.Vector Double) AudioMonad a
  -> IO (Either e a)
runAudioIteratee fp i = runAudioMonad $ enumAudioIteratee fp i >>= tryRun
