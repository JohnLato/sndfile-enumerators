{-# LANGUAGE FlexibleContexts #-}

module Sound.Iteratee.Codecs.Raw (
  RawCodec (..)
)

where

import           Sound.Iteratee.Base
import           Sound.Iteratee.Codecs.Common

data RawCodec = RawCodec

instance WritableAudio RawCodec where
  emptyState         RawCodec   = error "emptyState not defined for Raw files"
  initState          RawCodec _ = error "initState not defined for Raw files"
  supportedBitDepths RawCodec   = Any
  fileType           RawCodec   = Raw
