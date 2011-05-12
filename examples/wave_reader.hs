-- Read a wave file and return some information about it.
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Prelude as P

import Sound.Iteratee.Codecs.Wave
import qualified Data.Vector.Storable as V
import qualified Data.IntMap as IM
import           Data.Iteratee as I
import Data.Word (Word8)
import Control.Monad.CatchIO
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import System

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Usage: wave_reader FileName"
    fname:xs -> do
      putStrLn $ "Reading file: " ++ fname
      fileDriverRandom (waveReader >>= test) fname
      return ()

-- Use the collection of [WAVEDE] returned from waveReader to
-- do further processing.  The IntMap has an entry for each type of chunk
-- in the wave file.  Read the first format chunk and disply the 
-- format information, then use the dictProcessData function
-- to enumerate over the maxIter iteratee to find the maximum value
-- (peak amplitude) in the file.
test :: Maybe (IM.IntMap [WAVEDE])
  -> Iteratee (V.Vector Word8) IO ()
test Nothing = liftIO $ putStrLn "No dictionary"
test (Just dict) = do
  fmtm <- dictReadFirstFormat dict
  liftIO . putStrLn $ show fmtm
  maxm <- dictProcessData_ 0 dict maxIter
  liftIO . putStrLn $ show maxm
  return ()

-- | As of now (ghc-7.0.2, mutable-iter-0.6, sndfile-enumerators-0.7)
--  ,this version is as fast as a low-level implementation
maxIter :: MonadCatchIO m => Iteratee (V.Vector Double) m Double
maxIter = foldl' (flip (max . abs)) 0
