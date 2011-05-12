{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, MultiParamTypeClasses #-}

module Sound.Iteratee.Base (
  -- * Types
  -- ** Internal types
  AudioStreamState (..),
  WritableAudio (..),
  AudioMonad,
  -- *** Functions to work with AudioMonad
  module Control.Monad.Trans.State,
  defaultChunkLength,
  -- ** Audio Format types
  AudioFormat (..),
  SupportedBitDepths (..),
  NumChannels,
  SampleRate,
  BitDepth,
  FrameCount,
  -- ** File Format Types
  SupportedFileFormat (..)
)

where

import Prelude as P

import           Control.Monad.Trans.State
import           Control.Monad.IO.Class
import           System.IO
import           Data.Nullable
import           Data.NullPoint
import           Data.Iteratee.Base.ReadableChunk
import qualified Data.Vector.Storable as V
import           Data.Word

import           Foreign.Marshal.Utils
import           Foreign.ForeignPtr
import           Foreign.Storable

-- |Information about the AudioStream
data AudioStreamState =
  WaveState !(Maybe Handle) !(Maybe AudioFormat) !Integer !Integer !Integer -- ^ Handle, format, Total bytes written, data bytes written, data chunklen offset
  | NoState
  deriving (Eq, Show)

-- | An enumeration of all file types supported for reading and writing.
data SupportedFileFormat = Raw
                           | Wave
                           deriving (Show, Enum, Bounded, Eq)

-- | Common functions for writing audio data
class WritableAudio a where
  emptyState :: a -> AudioStreamState
  initState ::  a -> Handle -> AudioStreamState
  supportedBitDepths :: a -> SupportedBitDepths
  fileType           :: a -> SupportedFileFormat

-- | Audio monad stack (for writing files)
type AudioMonad = StateT AudioStreamState IO

-- | Format of audio data
data AudioFormat = AudioFormat {
  numberOfChannels :: NumChannels, -- ^Number of channels in the audio data
  sampleRate :: SampleRate, -- ^Sample rate of the audio data
  bitDepth :: BitDepth -- ^Bit depth of the audio data
  } deriving (Show, Eq)

type NumChannels = Integer
type SampleRate  = Integer
type BitDepth    = Integer
type FrameCount  = Integer

data SupportedBitDepths = Any | Supported [BitDepth]

defaultChunkLength :: Int
defaultChunkLength = 8190

instance V.Storable a => Nullable (V.Vector a) where
  nullC = V.null

instance V.Storable a => NullPoint (V.Vector a) where
  empty = V.empty

instance ReadableChunk (V.Vector Word8) Word8 where
  readFromPtr src blen = liftIO $ do
    fp <- mallocForeignPtrBytes blen
    withForeignPtr fp $ \dest -> copyBytes dest src blen
    return $ V.unsafeFromForeignPtr fp 0 blen
