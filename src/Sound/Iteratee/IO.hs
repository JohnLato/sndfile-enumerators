-- |Random and Binary IO with generic Iteratees.

module Sound.Iteratee.IO(
  -- * File enumerators
  enumRandom
)

where

import Sound.Iteratee.Base

import Data.Iteratee.Base
import Data.Iteratee.Base.StreamChunk
import Data.Iteratee.Binary()
import Data.Iteratee.IO.Base
import Data.Int
import Control.Monad.State
import Control.Exception.Extensible

import Foreign.Ptr
import Foreign.Marshal.Alloc

import System.IO


-- ------------------------------------------------------------------------
-- Binary Random IO enumerators

-- |The enumerator of a Handle: a variation of enumHandle that
-- supports RandomIO (seek requests).
-- this version uses handles for compatibility
enumRandom :: ReadableChunk s el =>
                  Handle ->
                  EnumeratorGM s el AudioMonad a
enumRandom h iter = lift get >>= \st ->
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
   off' <- (try $ hSeek h AbsoluteSeek
             (fromIntegral off)) :: IO (Either SomeException ())
   case off' of
    Left _errno -> evalStateT (unIM $ enumErr "IO error" iter') sst
    Right _     -> loop sst (off, 0) (Cont c) p
  loop _sst (off,len) _iter' _p | off `seq` len `seq` False = undefined
  loop sst (off,len) iter'@(Cont step) p = do
   n <- (try $ hGetBuf h p buffer_size) :: IO (Either SomeException Int)
   case n of
    Left _errno -> evalStateT (unIM $ step (Error "IO error")) sst
    Right 0 -> return iter'
    Right n' -> do
         s <- readFromPtr p (fromIntegral n')
         (im, sst') <- runStateT (unIM $ step (Chunk s)) sst
	 loop sst' (off + fromIntegral len,fromIntegral n') im p

