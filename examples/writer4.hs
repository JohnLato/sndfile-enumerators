-- Copy data from one file to another
-- compile with -funfolding-use-threshold=64
-- This uses iteratee-stm to improve throughput.
-- Certain patterns don't work with iteratee-stm (e.g. seek), so we
-- can only fork `enumAudioIteratee`.

{-# LANGUAGE BangPatterns #-}

module Main where

import           Data.Iteratee
import           Data.Iteratee.STM
import qualified Data.Vector.Storable as V
import           Sound.Iteratee.Codecs.Wave
import           Sound.Iteratee
import qualified Data.IntMap as IM
import           Data.Word (Word8)
import           Control.Monad.Trans
import           System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    fname:oname:xs -> do
      putStrLn $ "Reading file: " ++ fname
      fmtm <- fileDriverAudio (waveReader >>= getDict) fname
      let e2 = forkEnum 4 $ enumAudioIteratee fname
      runAudioMonad $ e2 (writer oname fmtm) >>= run
      return ()
    _ -> putStrLn "Usage: wave_writer ReadFile WriteFile"

-- Write out a new file with the specified format.
writer :: FilePath -> Maybe AudioFormat -> Iteratee (V.Vector Double) AudioMonad ()
writer fp fmtm = do
  liftIO $ putStrLn $ show fmtm
  maybe (error "No format") (writeWave fp) fmtm
  return ()

getDict Nothing = return Nothing
getDict (Just dict) = dictReadFirstFormat dict

