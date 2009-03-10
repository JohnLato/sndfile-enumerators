-- |Random and Binary IO with generic Iteratees.

module Sound.Iteratee.IO(
  -- * File enumerators
  enum_fd_random
)

where

import Sound.Iteratee.Base

import Data.Iteratee.Base
import Data.Iteratee.Base.StreamChunk
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
enum_fd_random :: ReadableChunk s el =>
                  Fd ->
                  EnumeratorGM s el AudioMonad a
enum_fd_random fd iter = lift get >>= \st ->
 IM $ liftIO $ allocaBytes (fromIntegral buffer_size) (loop st (0,0) iter)
 where
  buffer_size = 4096
  -- the second argument of loop is (off,len), describing which part
  -- of the file is currently in the buffer 'p'
  loop :: (ReadableChunk s el) =>
          AudioStreamState ->
          (FileOffset,Int) ->
          IterateeG s el AudioMonad a ->
	  Ptr el ->
          IO (IterateeG s el AudioMonad a)
  loop _sst _pos iter'@Done{} _p = return iter'
  loop sst pos@(off,len) (Seek off' c) p |
    off <= off' && off' < off + fromIntegral len =	-- Seek within buffer p
    do
    let local_off = fromIntegral $ off' - off
    str <- readFromPtr (p `plusPtr` local_off) (len - local_off)
    (im, s) <- runStateT (unIM $ c (Chunk str)) sst
    loop s pos im p
  loop sst _pos iter'@(Seek off c) p = do -- Seek outside the buffer
   off' <- myfdSeek fd AbsoluteSeek (fromIntegral off)
   case off' of
    Left _errno -> evalStateT (unIM $ enumErr "IO error" iter') sst
    Right off''  -> loop sst (off'',0) (Cont c) p
  loop _sst (off,len) _iter' _p | off `seq` len `seq` False = undefined
  loop sst (off,len) iter'@(Cont step) p = do
   n <- myfdRead fd (castPtr p) buffer_size
   case n of
    Left _errno -> evalStateT (unIM $ step (Error "IO error")) sst
    Right 0 -> return iter'
    Right n' -> do
         s <- readFromPtr p (fromIntegral n')
         (im, sst') <- runStateT (unIM $ step (Chunk s)) sst
	 loop sst' (off + fromIntegral len,fromIntegral n') im p

