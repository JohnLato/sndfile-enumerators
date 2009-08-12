-- Read a wave file and return some information about it.

{-# LANGUAGE BangPatterns, FlexibleContexts #-}
module Main where

import Prelude as P

import Data.Iteratee
import Sound.Iteratee as CV
import qualified Data.StorableVector as SV
import qualified Data.IntMap as IM
import Data.Word (Word8)
import Data.List (transpose)
import qualified Data.TypeLevel.Num as T
import Control.Monad.Trans
import Control.Applicative
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
-- to enumerate over the maxIter iteratee to find the maximum value
-- (peak amplitude) in the file.
test :: Maybe (IM.IntMap [WAVEDE]) -> IterateeG V Word8 IO ()
test Nothing = lift $ putStrLn "No dictionary"
test (Just dict) = do
  fmtm <- dictReadFirstFormat dict
  lift . print $ fmtm
  case fmtm of
    Just fmt -> do
      case numberOfChannels fmt of
        1 -> (dictProcessData 0 dict . joinI . channelizeStream T.d1 $
                maxIter (undefined :: Mono T.D1 Double)) >>= lift . print
        2 -> (dictProcessData 0 dict . joinI . channelizeStream T.d2 $
                maxIter (undefined :: Stereo T.D2 Double)) >>= lift . print
        n -> T.reifyIntegral n $ \n' ->
               (dictProcessData 0 dict . joinI . channelizeStream n' $
               maxIter (undefined :: ListVector s Double)) >>= lift . print
    Nothing -> liftIO $ print "no format"
  return ()

maxIter :: (Monad m,
            Channelized s c (Double -> Double -> Double),
            Channelized s c Double,
            Channelized s c (SV.Vector Double)) =>
  c s Double
  -> IterateeG [] (ChannelizedVector s Double) m (c s Double)
maxIter _ = Data.Iteratee.foldl' (CV.foldl' f) (toC $ repeat 0)
  where
    f = pure $ flip (max . abs)
