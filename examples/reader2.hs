-- Read a wave file and return some information about it.
-- This uses iteratee-stm for concurrency and to improve throughput.
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Prelude as P

import           Sound.Iteratee
import qualified Data.Vector.Storable as V
import           IterX
import           System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Usage: wave_reader FileName"
    fname:_ -> do
      putStrLn $ "Reading file: " ++ fname
      e <- runAudioMonad $ foldG maxf1 0
            $ mapsG (V.toList . nfChunkData) $ genAudio fname
      print e

maxf1 :: Monad m => Double -> Double -> m Double
maxf1 (!s') n = return $ max s' (abs n)
