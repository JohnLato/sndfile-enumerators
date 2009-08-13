module Sound.Iteratee.Codecs.Raw (
  RawCodec (..)
  ,readRaw
)

where

import Sound.Iteratee.Base
import Sound.Iteratee.Instances()
import Sound.Iteratee.Codecs.Common
import Data.Iteratee
import qualified Data.StorableVector as SV
import Data.Word
import Control.Monad.Trans

type V = SV.Vector

data RawCodec = RawCodec

instance WritableAudio RawCodec where
  emptyState         RawCodec   = error "emptyState not defined for Raw files"
  initState          RawCodec _ = error "initState not defined for Raw files"
  supportedBitDepths RawCodec   = Any
  fileType           RawCodec   = Raw

readRaw :: (MonadIO m, Functor m) =>
           AudioFormat -> EnumeratorGMM V Word8 V Double m a
readRaw fmt iter_dub = let iter = convStream (convFunc fmt) iter_dub in
  return . joinI $ iter

