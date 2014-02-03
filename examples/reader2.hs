-- Read a wave file and return some information about it.
-- This uses iteratee-stm for concurrency and to improve throughput.
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Prelude as P

import           Sound.Iteratee
import qualified Data.Vector.Storable as V
import           IterX
import           IterX.Fusion
import           System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Usage: wave_reader FileName"
    fname:_ -> do
      putStrLn $ "Reading file: " ++ fname
      e <- runAudioMonad $ runFold
            (maps nfChunkData . foldUnfolding unfoldVec $ folding maxf1 0)
            $ genAudio fname
      print e

maxf1 :: Double -> Double -> Double
maxf1 (!s') n = max s' (abs n)
