-- Read a wave file and return some information about it.

{-# LANGUAGE BangPatterns #-}
module Main where

import Data.Iteratee
import Sound.Iteratee.Codecs.Wave
import qualified Data.StorableVector as SV
import qualified Data.IntMap as IM
import Data.Word (Word8)
import Control.Monad.Trans
import System

type V = SV.Vector

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
-- to enumerate over the max_iter iteratee to find the maximum value
-- (peak amplitude) in the file.
test :: Maybe (IM.IntMap [WAVEDE]) -> IterateeG V Word8 IO ()
test Nothing = lift $ putStrLn "No dictionary"
test (Just dict) = do
  fmtm <- dictReadFirstFormat dict
  lift . putStrLn $ show fmtm
  maxm <- dictProcessData 0 dict max_iter
  lift . putStrLn $ show maxm
  return ()

-- an iteratee that calculates the maximum value found so far.
-- this could be written with snext as well, however it is more
-- efficient to operate on an entire chunk at once.
max_iter :: IterateeG V Double IO Double
max_iter = m' 0
  where
  m' acc = IterateeG (step acc)
  step acc (Chunk xs) | SV.null xs = return $ Cont (m' acc) Nothing
  step acc (Chunk xs) = return $ Cont
                        (m' $! SV.foldl' (max . abs) acc xs)
                        Nothing
  step acc str = return $ Done acc str
