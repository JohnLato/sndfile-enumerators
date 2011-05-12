-- Copy data from one file to another
-- compile with -funfolding-use-threshold=64

{-# LANGUAGE BangPatterns #-}

module Main where

import           Data.Iteratee
import qualified Data.Vector.Storable as V
import           Sound.Iteratee.Codecs.Wave
import           Sound.Iteratee
import qualified Data.IntMap as IM
import           Data.Word (Word8)
import           Control.Monad.Trans
import           System

main :: IO ()
main = do
  args <- getArgs
  case args of
    fname:oname:xs -> do
      putStrLn $ "Reading file: " ++ fname
      fileDriverAudio (waveReader >>= writer oname) fname
      return ()
    _ -> putStrLn "Usage: wave_writer ReadFile WriteFile"

-- Use the collection of [WAVEDE] returned from waveReader to
-- do further processing.  In this case, write out a new file with the
-- same format.
writer :: FilePath -> Maybe (IM.IntMap [WAVEDE]) -> Iteratee (V.Vector Word8) AudioMonad ()
writer _ Nothing = liftIO $ putStrLn "No dictionary"
writer fp (Just dict) = do
  fmtm <- dictReadFirstFormat dict
  liftIO $ putStrLn $ show fmtm
  maybe (error "No format")
        (\fmt -> dictProcessData 0 dict $ writeWave fp fmt)
        fmtm
  return ()

