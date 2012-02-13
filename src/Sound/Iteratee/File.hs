{-# LANGUAGE RankNTypes, FlexibleContexts #-}

module Sound.Iteratee.File (
  getFormat
 ,getAudioInfo
 ,runAudioIteratee
 ,tryRunAudioIteratee
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
import qualified Data.Vector.Storable as V

import           Control.Exception
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Control
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
  Just Wave -> fileDriverAudio (waveReader >>=
             maybe (return Nothing) dictSoundInfo) fp
  Just Raw  -> return Nothing
  _         -> return Nothing -- could try everything and see what matches...
{-# INLINE getAudioInfo #-}

enumAudioIterateeWithFormat ::
  (MonadIO m, MonadBaseControl IO m, Functor m)
  => FilePath
  -> (AudioFormat -> Iteratee (V.Vector Double) m a)
  -> m (Iteratee (V.Vector Double) m a)
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
{-# INLINE enumAudioIterateeWithFormat #-}

enumAudioIteratee ::
  (MonadIO m, MonadBaseControl IO m, Functor m)
  => FilePath
  -> Iteratee (V.Vector Double) m a
  -> m (Iteratee (V.Vector Double) m a)
enumAudioIteratee fp i = enumAudioIterateeWithFormat fp (const i)
{-# INLINE enumAudioIteratee #-}

runAudioIteratee ::
  FilePath
  -> Iteratee (V.Vector Double) AudioMonad a
  -> IO a
runAudioIteratee fp i = runAudioMonad $ enumAudioIteratee fp i >>= run
{-# INLINE runAudioIteratee #-}

tryRunAudioIteratee :: Exception e
  => FilePath
  -> Iteratee (V.Vector Double) AudioMonad a
  -> IO (Either e a)
tryRunAudioIteratee fp i = runAudioMonad $ enumAudioIteratee fp i >>= tryRun
