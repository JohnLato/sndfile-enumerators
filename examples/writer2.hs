-- Copy data from two separate files to another.

{-# LANGUAGE BangPatterns, RankNTypes #-}
module Main where

import Data.MutableIter
import qualified Data.MutableIter.IOBuffer as IB
import Sound.Iteratee.Codecs.Wave
import Sound.Iteratee
import qualified Data.Iteratee as I
import qualified Data.IntMap as IM
import Data.Word (Word8)
import Foreign.Storable (Storable)
import Control.Monad.Trans
import System

import System.IO
import Control.Monad.CatchIO as CIO
import Control.Monad

file2Driver :: (MonadCatchIO m, Functor m) =>
  (forall r. Maybe (IM.IntMap [WAVEDE])
    -> MIteratee (IB.IOBuffer r Word8) m (MIteratee (IB.IOBuffer r Double) m a))
  -> FilePath
  -> FilePath
  -> m a
file2Driver i f1 f2 = CIO.bracket
  (liftIO $ (openBinaryFile f1 ReadMode >>= \h1 -> openBinaryFile f2 ReadMode >>= \h2 -> return (h1,h2)))
  (liftIO . (\(h1, h2) -> hClose h1 >> hClose h2))
  (\(h1,h2) -> (I.run . unwrap) =<< (I.run . unwrap) =<<
    ( enumHandleRandom defaultChunkLength h1 (waveReader >>= i) >>= \i2 -> enumHandleRandom defaultChunkLength h2 (waveReader >>= \mdict -> i2 >>= embedProc mdict)))

main :: IO ()
main = do
  args <- getArgs
  case args of
    fname1:fname2:oname:xs -> do
      putStrLn $ "Reading file1 " ++ fname1
      putStrLn $ "Reading file2 " ++ fname2
      runAudioMonad $ file2Driver (writer oname) fname1 fname2
      return ()
    _ -> putStrLn "Usage: wave_writer ReadFile1 ReadFile2 WriteFile"


embedProc :: (MonadCatchIO m, Functor m) => Maybe (IM.IntMap [WAVEDE]) -> MIteratee (IB.IOBuffer r Double) m a -> MIteratee (IB.IOBuffer r Word8) m (MIteratee (IB.IOBuffer r Double) m a)
embedProc Nothing i = error "No dictionary"
embedProc (Just dict) i = dictProcessData 0 dict i

-- Use the collection of [WAVEDE] returned from waveReader to
-- do further processing.  The IntMap has an entry for each type of chunk
-- in the wave file.  Read the first format chunk and disply the 
-- format information, then use the dictProcessData function
-- to enumerate over the max_iter iteratee to find the maximum value
-- (peak amplitude) in the file.
writer :: FilePath -> Maybe (IM.IntMap [WAVEDE]) -> MIteratee (IB.IOBuffer r Word8) AudioMonad (MIteratee (IB.IOBuffer r Double) AudioMonad ())
writer _ Nothing = error "No Dictionary"
writer fp (Just dict) = do
  fmtm <- dictReadFirstFormat dict
  liftIO $ putStrLn $ show fmtm
  maybe (error "No format")
        (\fmt -> dictProcessData 0 dict $ writeWave fp fmt)
        fmtm

