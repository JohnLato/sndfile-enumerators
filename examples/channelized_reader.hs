-- Read a wave file and return some information about it.
-- This program demonstrates how to use the Channelized features.

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
--
-- Unfortunately I do not yet know how to make a reification function to
-- process the data, so it's necessary to do a case on the number of channels.
-- Of course one could just use ListVector as in the final case, but that
-- is much less efficient.
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

-- |Calculate the maximum value of an iteratee of ChannelizedVectors.
-- Note that this is completely polymorphic over channel numbers.
maxIter :: (Monad m,
    Channelized s c (Double -> Double -> Double),
    Channelized s c Double,
    Channelized s c (SV.Vector Double)) =>
  c s Double
  -> IterateeG [] (ChannelizedVector s Double) m (c s Double)
maxIter _ = Data.Iteratee.foldl' (CV.foldl' f) (toC $ repeat 0)
  where
    f = pure $ flip (max . abs)
