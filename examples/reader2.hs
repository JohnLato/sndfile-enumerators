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

{-# RULES
-- this rule seems to hurt, needs a little more investigation?
-- "<iterx>maps/foldingM" forall f g s. maps f (foldingM g s) = foldingM (\b -> g b . f) s
"<iterx>maps/maps_2" forall x f g. maps f (maps g x) = maps (g . f) x
      #-}

{-# INLINE t1 #-}
-- the ForceSpecConstr in foldUnfolding helps a lot, but it's still
-- 25% slower than the vector version.
--
-- And the core looks really nice, but Unfold can be improved.
--
-- foldFoldable is slow, because the inner loop isn't unboxed?  WTF?
t1 :: FoldM AudioMonad NormFormattedChunk Double
-- t1 = maps nfChunkData . foldUnfolding2 unfold2Vec . maps abs $ folding max 0
-- t1 = maps nfChunkData . maps unfold2Vec . foldUnfolding2a . maps abs $ folding max 0

-- this is nearly as fast as the plain vector code.  removing the
-- `runIdentity` makes it a bit slower, but it's still pretty fast.
-- t1 = maps nfChunkData . maps (\v -> runIdentity $ stepFold (foldUnfolding2 unfold2Vec . maps abs $ folding max 0) v >>= getFold) $ folding max 0
-- t1 = maps nfChunkData . unfolding unfold2Vec . maps abs $ folding max 0
t1 = maps nfChunkData . unfolding unfold2Vec . maps abs $ folding max 0
-- t1 = maps nfChunkData . foldUnfolding unfoldVec $ folding maxf1 0
-- t1 = maps nfChunkData . maps (V.maximum . V.map abs) $ folding max 0

maxf1 :: Double -> Double -> Double
maxf1 (!s') n = max s' (abs n)
