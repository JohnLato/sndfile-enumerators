-- Copy data from one file to another

{-# LANGUAGE BangPatterns #-}
module Main where

import Data.Iteratee
import Sound.Iteratee.Codecs.Wave
import Sound.Iteratee
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
    fname:oname:xs -> do
      putStrLn $ "Reading file: " ++ fname
      fileDriverAudio (waveReader >>= writer oname) fname
      --runAudioMonad $ run $ writer2 oname
      return ()
    _ -> putStrLn "Usage: wave_writer ReadFile WriteFile"

-- Use the collection of [WAVEDE] returned from waveReader to
-- do further processing.  The IntMap has an entry for each type of chunk
-- in the wave file.  Read the first format chunk and disply the 
-- format information, then use the dictProcessData function
-- to enumerate over the max_iter iteratee to find the maximum value
-- (peak amplitude) in the file.
writer :: FilePath -> Maybe (IM.IntMap [WAVEDE]) -> IterateeG V Word8 AudioMonad ()
writer _ Nothing = liftIO $ putStrLn "No dictionary"
writer fp (Just dict) = do
  fmtm <- dictReadFirstFormat dict
  liftIO $ putStrLn $ show fmtm
  maybe (error "No format")
        (\fmt -> dictProcessData 0 dict $ writeWave fp fmt)
        fmtm
  return ()

-- |Write an empty wave file to the specified file
writer2 :: FilePath -> IterateeG V Double AudioMonad ()
writer2 fp = do
  let fmt = AudioFormat 2 44100 16
  joinIM $ enumPure1Chunk SV.empty $ writeWave fp fmt
  return ()
