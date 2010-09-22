-- Read a wave file and return some information about it.

{-# LANGUAGE BangPatterns #-}
module Main where

import Prelude as P

import Sound.Iteratee.Codecs.Wave
import Data.MutableIter
import Data.MutableIter.IOBuffer (IOBuffer)
import qualified Data.MutableIter.IOBuffer as IB
import qualified Data.IntMap as IM
import qualified Data.Iteratee as I
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
      fileDriverRandom (2^14) (waveReader >>= test) fname
      return ()

-- Use the collection of [WAVEDE] returned from waveReader to
-- do further processing.  The IntMap has an entry for each type of chunk
-- in the wave file.  Read the first format chunk and disply the 
-- format information, then use the dictProcessData function
-- to enumerate over the max_iter iteratee to find the maximum value
-- (peak amplitude) in the file.
test :: Maybe (IM.IntMap [WAVEDE])
  -> MIteratee (IOBuffer r Word8) IO ()
test Nothing = liftIO $ putStrLn "No dictionary"
test (Just dict) = do
  fmtm <- dictReadFirstFormat dict
  liftIO . putStrLn $ show fmtm
  maxm <- dictProcessData_ 0 dict max_iter
  liftIO . putStrLn $ show maxm
  return ()

max_iter2 :: MonadCatchIO m => MIteratee (IOBuffer r Double) m Double
max_iter2 = foldl' (flip (max . abs)) 0

-- |This version is faster, but lower-level
max_iter :: MIteratee (IOBuffer r Double) IO Double
max_iter = m' 0
  where
    m' !acc = liftI (step acc)
    step acc (I.Chunk buf) = guardNull buf (m' acc) $ do
      val <- lift $ IB.foldl' (flip (max . abs)) acc buf
      m' val
    step acc str = idone acc str
