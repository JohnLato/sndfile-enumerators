{-# LANGUAGE ExistentialQuantification #-}

module Sound.Iteratee.Codecs (
  module Sound.Iteratee.Codecs.Wave,
  getWriter,
  Codec (..),
  getCodec,
  module Sound.Iteratee.Codecs.Common
)

where

import Sound.Iteratee.Base
import Sound.Iteratee.Codecs.Wave
import Sound.Iteratee.Codecs.Raw
import Sound.Iteratee.Codecs.Common (RawFormattedChunk(..), NormFormattedChunk(..), nfChunkData)
import IterX

import qualified Data.Vector.Storable as V

-- |Get a writer iteratee for a SupportedFileFormat
getWriter ::
  SupportedFileFormat
  -> FilePath
  -> AudioFormat
  -> Consumer AudioMonad (V.Vector Double)
getWriter Wave = writeWave
getWriter Raw  = error "No writer defined for Raw format"

-- |An existentially-wrapped codec.  This exists in order to get an arbitrary
-- codec (and associated information, such as bit depths)
-- from a SupportedFileFormat.
data Codec = forall a. WritableAudio a => Codec a

instance WritableAudio Codec where
  emptyState         (Codec a) = emptyState a
  initState          (Codec a) = initState a
  supportedBitDepths (Codec a) = supportedBitDepths a
  fileType           (Codec a) = fileType a

getCodec :: SupportedFileFormat -> Codec
getCodec Wave = Codec WaveCodec
getCodec Raw  = Codec RawCodec
