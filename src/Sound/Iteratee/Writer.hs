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

import System.Posix

-- |Process a file using the given IterateeGM.  This function wraps
-- enum_fd_random as a convenience.

runAudioMonad :: AudioMonad a -> IO a
runAudioMonad am = do
  (a, s) <- runStateT am NoState
  case s of
    NoState     -> return a
    WaveState{} -> runWaveAM (put s >> return a)

fileDriverAudio :: ReadableChunk s el => IterateeGM s el AudioMonad a ->
               FilePath ->
               IO (Either (String, a) a)
fileDriverAudio iter filepath = do
  fd <- openFd filepath ReadOnly Nothing defaultFileFlags
  result <- runAudioMonad (unIM $ (enum_fd_random fd >. enumEof) ==<< iter)
  closeFd fd
  print_res result
 where
  print_res (Done a (Error err)) = return $ Left (err, a)
  print_res (Done a _) = return $ Right a
  print_res (Cont _) = return $ Left ("Iteratee unfinished", undefined)
  print_res (Seek _ _) = return $ Left ("Iteratee unfinished", undefined)
                           