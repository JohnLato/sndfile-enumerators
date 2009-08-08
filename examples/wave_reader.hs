-- Read a wave file and return some information about it.

{-# LANGUAGE BangPatterns #-}
module Main where

import Prelude as P

import Data.Iteratee
import Sound.Iteratee as CV
import qualified Data.StorableVector as SV
import qualified Data.IntMap as IM
import Data.Word (Word8)
import Data.List (transpose)
import Control.Monad.Trans
import Control.Parallel.Strategies
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
  case fmtm of
    Just fmt -> do
      maxm <- dictProcessData 0 dict . joinI . deMux fmt $ max_iter
      lift . putStrLn $ show maxm
    Nothing -> liftIO $ print "no format"
  return ()

-- |This version is faster, but lower-level
max_iter :: IterateeG [] (ChannelizedVector Double) IO [Double]
max_iter = m' (repeat 0)
  where
  m' acc = IterateeG (step acc)
  step acc (Chunk []) = return $ Cont (m' acc) Nothing
  step acc (Chunk xs) = return $ Cont (m' $! newacc) Nothing
    where
      newacc = (map (P.foldl1 f) . transpose . map (CV.foldl' (repeat f) acc) $ xs)
  step acc str = return $ Done acc str
  f = flip (max . abs)

-- |This version is slower, but high-level and easier to understand.
max2 :: IterateeG V Double IO Double
max2 = Data.Iteratee.foldl' (flip (max . abs)) 0
