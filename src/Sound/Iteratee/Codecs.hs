module Sound.Iteratee.Codecs (
  module Sound.Iteratee.Codecs.Wave,
  getWriter
)

where

import Sound.Iteratee.Base
import Sound.Iteratee.Codecs.Wave
import Data.Iteratee

import qualified Data.StorableVector as SV

getWriter :: SupportedFileFormat ->
             FilePath ->
             AudioFormat ->
             IterateeG SV.Vector Double AudioMonad ()
getWriter Wave = writeWave
getWriter Raw  = error "No writer defined for Raw format"

