module Sound.Iteratee.Codecs.Raw (
  readRaw
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

readRaw :: (MonadIO m, Functor m) =>
           AudioFormat -> EnumeratorGMM V Word8 V Double m a
readRaw fmt iter_dub = let iter = convStream (conv_func fmt) iter_dub in
  return . joinI $ iter

