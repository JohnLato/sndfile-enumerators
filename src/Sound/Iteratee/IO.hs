{-# LANGUAGE ScopedTypeVariables #-}

-- |Random and Binary IO with generic Iteratees.

module Sound.Iteratee.IO(
  -- * File enumerators
  enumAudioFile
)

where

import Sound.Iteratee.Base

import Data.Iteratee.Base
import Data.Iteratee.Base.StreamChunk
import Data.Iteratee.Binary()
import Data.Iteratee.IO.Base
import Data.Int
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Exception.Extensible

import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal.Alloc

import System.IO


-- ------------------------------------------------------------------------
-- Binary Random IO enumerators

-- |The enumerator of a Handle: a variation of enumHandle that
-- supports RandomIO (seek requests).
-- this version uses handles for compatibility
enumAudioFile :: forall a s el.(ReadableChunk s el) =>
                 Handle ->
                 EnumeratorGM s el AudioMonad a
enumAudioFile h iter = do
 st <- get
 (iter', st') <- liftIO $ allocaBytes buffer_size (loop st (0,0) iter)
 put st'
 return iter'
 where
  buffer_size = fromIntegral $ 2048 - (mod 2048 $ sizeOf (undefined :: el))
  -- the second argument of loop is (off,len), describing which part
  -- of the file is currently in the buffer 'p'
  loop :: AudioStreamState ->
          (FileOffset,Int) ->
          IterateeG s el AudioMonad a ->
	  Ptr el ->
          IO (IterateeG s el AudioMonad a, AudioStreamState)
  loop _sst (off,len) _iter' _p | off `seq` len `seq` False = undefined
  loop sst (off,len) iter' p = do
    n <- try $ hGetBuf h p buffer_size :: IO (Either SomeException Int)
    case n of
      Left _errno -> runStateT (enumErr "IO error" iter') sst
      Right 0  -> return (iter', sst)
      Right n' -> do
         s <- readFromPtr p (fromIntegral n')
         (igv, sst') <- runStateT (runIter iter' (Chunk s)) sst
         check sst' (off + fromIntegral len,fromIntegral n') igv p
  seekTo sst pos@(off,len) off' iter' p
    | off <= off' && off' < off + fromIntegral len =	-- Seek within buffer
    do
    let local_off = fromIntegral $ off' - off
    str      <- readFromPtr (p `plusPtr` local_off) (len - local_off)
    (igv, s) <- runStateT (runIter iter' (Chunk str)) sst
    check s pos igv p
  seekTo sst _pos off iter' p = do		-- Seek outside the buffer
    off' <- try $ hSeek h AbsoluteSeek
            (fromIntegral off) :: IO (Either SomeException ())
    case off' of
      Left  _errno -> runStateT (enumErr "IO error" iter') sst
      Right _      -> loop sst (off, 0) iter' p
  check sst   _ (Done x _) _ = return (lift (put sst) >> return x, sst)
  check sst o (Cont k Nothing)           p = loop sst o k p
  check sst o (Cont k (Just (Seek off))) p = seekTo sst o off k p
  check sst _ (Cont _ (Just e))          _ = return $ (throwErr e, sst)

