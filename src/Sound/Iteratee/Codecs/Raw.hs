module Sound.Iteratee.Codecs.Raw (
  RawCodec (..)
  ,readRaw
)

where

import Sound.Iteratee.Base
import Sound.Iteratee.Codecs.Common
import           Data.Iteratee as I
import qualified Data.Vector.Storable as V

import Data.Word
import Control.Monad.CatchIO

data RawCodec = RawCodec

instance WritableAudio RawCodec where
  emptyState         RawCodec   = error "emptyState not defined for Raw files"
  initState          RawCodec _ = error "initState not defined for Raw files"
  supportedBitDepths RawCodec   = Any
  fileType           RawCodec   = Raw

readRaw ::
 (MonadCatchIO m, Functor m) =>
  AudioFormat
  -> Iteratee (V.Vector Double) m a
  -> Iteratee (V.Vector Word8) m a
readRaw fmt iter_dub = do
  joinI . convStream (convFunc fmt) $ iter_dub

