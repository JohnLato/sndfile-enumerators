{-# LANGUAGE ScopedTypeVariables
            ,FlexibleInstances
            ,DeriveGeneric
            ,MultiParamTypeClasses
            ,TypeFamilies
            ,DeriveDataTypeable #-}

module Sound.Iteratee.Base (
  -- * Types
  -- ** Internal types
  AudioStreamState (..),
  WritableAudio (..),
  AudioMonad,
  -- *** Functions to work with AudioMonad
  module Control.Monad.State.Strict,
  defaultChunkLength,
  -- ** Audio Format types
  AudioFormat (..),
  SupportedBitDepths (..),
  NumChannels,
  SampleRate,
  BitDepth,
  FrameCount,
  -- ** File Format Types
  SupportedFileFormat (..),
  -- ** Exceptions
  UnknownFileTypeException (..),
  CorruptFileException (..),
  MissingFormatException (..),
  -- * Audio iteratees
  getChannel
)

where

import Prelude as P

import           Control.Exception
import           Control.Monad.State.Strict
import           Control.Monad.IO.Class
import           System.IO
import           Data.Data
import           IterX
import           IterX.ReadableChunk
import           IterX.Exception
import           Data.ListLike.Vector.Storable ()
import qualified Data.Vector.Storable as V
import           Data.Word

import           Foreign.Marshal.Utils
import           Foreign.ForeignPtr
import           GHC.Generics

-- |Information about the AudioStream
data AudioStreamState =
  WaveState !(Maybe Handle) !(Maybe AudioFormat) !Integer !Integer !Integer -- ^ Handle, format, Total bytes written, data bytes written, data chunklen offset
  | NoState
  deriving (Eq, Show)

-- | An enumeration of all file types supported for reading and writing.
data SupportedFileFormat = Raw
                           | Wave
                           deriving (Show, Enum, Bounded, Eq, Data, Typeable)

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
  } deriving (Show, Eq, Data, Typeable, Generic)

type NumChannels = Int
type SampleRate  = Int
type BitDepth    = Int
type FrameCount  = Integer

data SupportedBitDepths = Any | Supported [BitDepth]

defaultChunkLength :: Int
defaultChunkLength = 8190

instance ReadableChunk (V.Vector Word8) where
  type El (V.Vector Word8) = Word8
  readFromPtr src blen = liftIO $ do
    fp <- mallocForeignPtrBytes blen
    withForeignPtr fp $ \dest -> copyBytes dest src blen
    return $ V.unsafeFromForeignPtr fp 0 blen
  fillFromCallback sz cb = do
      fp <- mallocForeignPtrBytes sz
      numFill <- withForeignPtr fp $ \p -> cb p sz
      return $! SizedS numFill $! V.unsafeFromForeignPtr fp 0 numFill
  empty = V.empty

-- | Operate on a single channel of an audio stream.
getChannel ::
  Monad m
  => Int     -- ^ number of channels
  -> Int     -- ^ channel index (1-based)
  -> Transducer (GenT (V.Vector Double) (StateT s m)) m (V.Vector Double) (V.Vector Double)
getChannel 1        m   = error "getChannel: not implemented"
getChannel numChans chn = error "getChannel: not implemented"

-- Audio Exceptions

data UnknownFileTypeException =
  UnknownFileTypeException deriving (Eq, Show, Typeable)

instance Exception UnknownFileTypeException where
  toException = iExceptionToException
  fromException = iExceptionFromException

instance IException UnknownFileTypeException where

data CorruptFileException = CorruptFileException deriving (Eq, Show, Typeable)

instance Exception CorruptFileException where
  toException = iExceptionToException
  fromException = iExceptionFromException

instance IException CorruptFileException where

data MissingFormatException =
  MissingFormatException deriving (Eq, Show, Typeable)

instance Exception MissingFormatException where
  toException = iExceptionToException
  fromException = iExceptionFromException

instance IException MissingFormatException where
