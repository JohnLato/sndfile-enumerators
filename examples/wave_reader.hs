-- Read a wave file and return some information about it.
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Prelude as P

import           Sound.Iteratee
import qualified Data.Vector.Storable as V
import           Data.Iteratee as I
import           Control.Monad.CatchIO
import           System

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Usage: wave_reader FileName"
    fname:_ -> do
      putStrLn $ "Reading file: " ++ fname
      e <- runAudioIteratee fname maxIter
      print e

-- | As of now (ghc-7.0.2, mutable-iter-0.6, sndfile-enumerators-0.7)
--  ,this version is as fast as a low-level implementation
maxIter :: MonadCatchIO m => Iteratee (V.Vector Double) m Double
maxIter = foldl' (flip (max . abs)) 0
