-- Read a wave file and return some information about it.
-- This uses iteratee-stm for concurrency and to improve throughput.
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -fsimpl-tick-factor=800 #-}
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
      e <- runAudioMonad $
            genAudio fname t1
      print e

{-# INLINE t1 #-}
-- the ForceSpecConstr in foldUnfolding helps a lot, but it's still about
-- half as fast as the vector version.  Which is a little slower than
-- the iteratee version.
--
-- but the maps abs $ folding max version is about as fast as folding maxf1 0,
-- so that's something...

-- maybe Monad-Control is slow?
-- maybe delimit stuff can be improved?  (almost certainly)
-- the vector is getting converted into a stream, and we're case'ing on
-- that... not good.
t1 :: FoldM AudioMonad NormFormattedChunk Double
t1 = maps nfChunkData . foldUnfolding unfoldVec $ maps abs $ folding max 0
-- t1 = maps nfChunkData . foldUnfolding unfoldVec $ folding maxf1 0
-- t1 = maps nfChunkData . maps (V.maximum . V.map abs) $ folding max 0

maxf1 :: Double -> Double -> Double
maxf1 (!s') n = max s' (abs n)
