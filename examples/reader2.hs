-- Read a wave file and return some information about it.
-- This uses iteratee-stm for concurrency and to improve throughput.
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -ddump-rule-firings -ddump-simpl -ddump-to-file #-}
module Main where

import Prelude as P

import           Sound.Iteratee
import qualified Data.Vector.Storable as V
import           IterX
import           IterX.Fusion
import           System.Environment
import Data.Functor.Identity

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Usage: wave_reader FileName"
    fname:_ -> do
      putStrLn $ "Reading file: " ++ fname
      e <- runAudioMonad $
            genAudio fname t1
      print e

{-# INLINE t1 #-}
--  This is now as fast as the vector code, yay!  But I'm not
--  entirely happy with the unfolding/Identity/Monad story.
--  but maybe I can leave that be for now...
t1 :: FoldM AudioMonad NormFormattedChunk Double
t1 = maps nfChunkData . unfolding unfold2Vec . maps abs $ folding max 0
-- t1 = maps nfChunkData . maps (V.maximum . V.map abs) $ folding max 0

maxf1 :: Double -> Double -> Double
maxf1 (!s') n = max s' (abs n)
