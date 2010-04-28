module Sound.Iteratee.Codecs.Raw (
  RawCodec (..)
  ,readRaw
)

where

import Sound.Iteratee.Base
import Sound.Iteratee.Codecs.Common
import Data.MutableIter
import qualified Data.MutableIter.IOBuffer as IB

import Data.Word
import Control.Monad.CatchIO
import Control.Monad.IO.Class

import Foreign.Marshal.Utils
import Foreign.Marshal.Array
import Foreign.ForeignPtr

type IOB = IB.IOBuffer

data RawCodec = RawCodec

instance WritableAudio RawCodec where
  emptyState         RawCodec   = error "emptyState not defined for Raw files"
  initState          RawCodec _ = error "initState not defined for Raw files"
  supportedBitDepths RawCodec   = Any
  fileType           RawCodec   = Raw

readRaw ::
 (MonadCatchIO m, Functor m) =>
  AudioFormat
  -> MIteratee (IOB r Double) m a
  -> MIteratee (IOB r Word8) m a
readRaw fmt iter_dub = do
  offp <- liftIO $ new 0 >>= newForeignPtr_
  bufp <- liftIO $ mallocArray defaultChunkLength >>= newForeignPtr_
  joinIob . convStream (convFunc fmt offp bufp) $ iter_dub

