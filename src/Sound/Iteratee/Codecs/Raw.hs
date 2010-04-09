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
import Control.Monad.Trans
import Control.Monad.Trans.Region

import Foreign.Marshal.Utils.Region
import Foreign.Marshal.Array.Region

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
  -> MIteratee (IOB (RegionT s m) Double) (RegionT s m) a
  -> MIteratee (IOB (RegionT s m) Word8) (RegionT s m) a
readRaw fmt iter_dub = do
  offp <- lift $ new 0
  bufp <- lift $ mallocArray defaultChunkLength
  joinIob . convStream (convFunc fmt offp bufp) $ iter_dub

