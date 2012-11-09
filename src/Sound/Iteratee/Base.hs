{-# LANGUAGE ScopedTypeVariables
            ,FlexibleInstances
            ,MultiParamTypeClasses
            ,DeriveDataTypeable #-}

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
import           Control.Monad.Trans.State
import           Control.Monad.IO.Class
import           System.IO
import           Data.Data
import           Data.Iteratee as I
import           Data.Iteratee.Base.ReadableChunk
import           Data.Iteratee.Exception ()
import           Data.ListLike.Vector.Storable ()
import qualified Data.Vector.Storable as V
import           Data.Word

import           Foreign.Marshal.Utils
import           Foreign.ForeignPtr

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
  } deriving (Show, Eq, Data, Typeable)

type NumChannels = Integer
type SampleRate  = Integer
type BitDepth    = Integer
type FrameCount  = Integer

data SupportedBitDepths = Any | Supported [BitDepth]

defaultChunkLength :: Int
defaultChunkLength = 8190

instance ReadableChunk (V.Vector Word8) Word8 where
  readFromPtr src blen = liftIO $ do
    fp <- mallocForeignPtrBytes blen
    withForeignPtr fp $ \dest -> copyBytes dest src blen
    return $ V.unsafeFromForeignPtr fp 0 blen
  empty = V.empty

-- | Operate on a single channel of an audio stream.
getChannel ::
  Monad m
  => Int     -- ^ number of channels
  -> Int     -- ^ channel index (1-based)
  -> Enumeratee (V.Vector Double) (V.Vector Double) m a
getChannel 1        m   = \i -> I.drop m >> convStream getChunk i
getChannel numChans chn = unfoldConvStream mkIter chn
 where
  mkIter drp = do
    I.drop drp
    buf <- getChunk
    let tlen = V.length buf
        (nlen,rest) = quotRem tlen numChans
        newbuf = V.generate nlen (\i -> V.unsafeIndex buf (i*numChans))
    return (rest, newbuf)

-- Audio Exceptions

data UnknownFileTypeException =
  UnknownFileTypeException deriving (Eq, Show, Typeable)

instance Exception UnknownFileTypeException where
  toException = iterExceptionToException
  fromException = iterExceptionFromException

instance IException UnknownFileTypeException where

data CorruptFileException = CorruptFileException deriving (Eq, Show, Typeable)

instance Exception CorruptFileException where
  toException = iterExceptionToException
  fromException = iterExceptionFromException

instance IException CorruptFileException where

data MissingFormatException =
  MissingFormatException deriving (Eq, Show, Typeable)

instance Exception MissingFormatException where
  toException = iterExceptionToException
  fromException = iterExceptionFromException

instance IException MissingFormatException where
