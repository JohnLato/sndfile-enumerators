module Sound.Iteratee.Codecs.SupportedFormats (
  -- * Supported types
  SupportedFileFormat (..)
)

where

-- | An enumeration of all file types supported for reading and writing.
data SupportedFileFormat = Wave
                           deriving (Show, Enum, Bounded, Eq)
