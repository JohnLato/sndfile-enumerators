-- |Random and Binary IO with generic Iteratees.

module Sound.Iteratee.IO(
  -- * Iteratee drivers
  file_driver_rb,
  -- * File enumerators
  enum_fd_random
)

where

import Sound.Iteratee.Base
import Data.Iteratee.Base
import Data.Iteratee.Binary()
import Data.Iteratee.IO.Base
import Data.Int
import Control.Monad.State

import System.Posix hiding (FileOffset)
import Foreign.Ptr
import Foreign.Marshal.Alloc

import System.IO (SeekMode(..))


-- ------------------------------------------------------------------------
-- Binary Random IO enumerators

-- |The enumerator of a POSIX File Descriptor: a variation of enum_fd that
-- supports RandomIO (seek requests)
-- TODO: modify this to support AudioStack instead of just IO.
enum_fd_random :: ReadableChunk s el =>
                  Fd ->
                  AudioStreamState ->
                  EnumeratorGM s el AudioStack a
enum_fd_random fd st iter =
 IM $ liftIO $ allocaBytes (fromIntegral buffer_size) (loop st (0,0) iter)
 where
  buffer_size = 4096
  -- the second argument of loop is (off,len), describing which part
  -- of the file is currently in the buffer 'p'
  loop :: (ReadableChunk s el) =>
          AudioStreamState ->
          (FileOffset,Int) ->
          IterateeG s el AudioStack a ->
	  Ptr el ->
          IO (IterateeG s el AudioStack a)
  loop _sst _pos iter'@IE_done{} _p = return iter'
  loop sst pos@(off,len) (IE_jmp off' c) p | 
    off <= off' && off' < off + fromIntegral len =	-- Seek within buffer p
    do
    let local_off = fromIntegral $ off' - off
    str <- readFromPtr (p `plusPtr` local_off) (len - local_off)
    (im, s) <- runStateT (unIM $ c (Chunk str)) sst
    loop s pos im p
  loop sst _pos iter'@(IE_jmp off c) p = do -- Seek outside the buffer
   off' <- myfdSeek fd AbsoluteSeek (fromIntegral off)
   case off' of
    Left _errno -> evalStateT (unIM $ enum_err "IO error" iter') sst
    Right off''  -> loop sst (off'',0) (IE_cont c) p
  loop _sst (off,len) _iter' _p | off `seq` len `seq` False = undefined
  loop sst (off,len) iter'@(IE_cont step) p = do
   n <- myfdRead fd (castPtr p) buffer_size
   case n of
    Left _errno -> evalStateT (unIM $ step (Err "IO error")) sst
    Right 0 -> return iter'
    Right n' -> do
         s <- readFromPtr p (fromIntegral n')
         (im, sst') <- runStateT (unIM $ step (Chunk s)) sst
	 loop sst' (off + fromIntegral len,fromIntegral n') im p

-- |Process a file using the given IterateeGM.  This function wraps
-- enum_fd_random as a convenience.
file_driver_rb :: ReadableChunk s el => IterateeGM s el AudioStack a ->
               FilePath ->
               IO (Either (String, a) a)
file_driver_rb iter filepath = do
  fd <- openFd filepath ReadOnly Nothing defaultFileFlags
  result <- evalStateT (unIM $ (enum_fd_random fd NoState >. enum_eof) ==<< iter) NoState
  closeFd fd
  print_res result
 where
  print_res (IE_done a (Err err)) = return $ Left (err, a)
  print_res (IE_done a _) = return $ Right a
  print_res (IE_cont _) = return $ Left ("Iteratee unfinished", undefined)
  print_res (IE_jmp _ _) = return $ Left ("Iteratee unfinished", undefined)
                           
