{-# LANGUAGE FlexibleContexts #-}

module Sound.Iteratee.Codecs.Raw (
  RawCodec (..)
)

where

import           Sound.Iteratee.Base
import           Sound.Iteratee.Codecs.Common

import qualified Data.Vector.Storable as V

import           Data.Word
import           Control.Monad.Trans.Control
import           Control.Monad.IO.Class

data RawCodec = RawCodec

instance WritableAudio RawCodec where
  emptyState         RawCodec   = error "emptyState not defined for Raw files"
  initState          RawCodec _ = error "initState not defined for Raw files"
  supportedBitDepths RawCodec   = Any
  fileType           RawCodec   = Raw
