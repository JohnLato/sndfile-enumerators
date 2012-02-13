-- Copy data from two separate files to another.

{-# LANGUAGE BangPatterns, RankNTypes, FlexibleContexts #-}
module Main where

import Sound.Iteratee.Codecs.Wave
import Sound.Iteratee
import           Data.Iteratee as I
import           Data.Iteratee.IO.Handle
import qualified Data.IntMap as IM
import qualified Data.Vector.Storable as V
import Data.Word (Word8)
import Foreign.Storable (Storable)
import Control.Monad.Trans
import System.Environment

import System.IO
import Control.Monad.IO.Class
import Control.Monad
import Control.Monad.Trans.Control
import Control.Exception.Lifted

file2Driver :: (MonadIO m, MonadBaseControl IO m, Functor m) =>
  (forall r. Maybe (IM.IntMap [WAVEDE])
    -> Iteratee (V.Vector Word8) m (Iteratee (V.Vector Double) m a))
  -> FilePath
  -> FilePath
  -> m a
file2Driver i f1 f2 = bracket
  (liftIO $ (openBinaryFile f1 ReadMode >>= \h1 -> openBinaryFile f2 ReadMode >>= \h2 -> return (h1,h2)))
  (liftIO . (\(h1, h2) -> hClose h1 >> hClose h2))
  (\(h1,h2) -> (I.run) =<< (I.run) =<<
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


embedProc :: (MonadIO m, MonadBaseControl IO m, Functor m) => Maybe (IM.IntMap [WAVEDE]) -> Iteratee (V.Vector Double) m a -> Iteratee (V.Vector Word8) m (Iteratee (V.Vector Double) m a)
embedProc Nothing i = error "No dictionary"
embedProc (Just dict) i = dictProcessData 0 dict i

-- Use the collection of [WAVEDE] returned from waveReader to
-- do further processing.  The IntMap has an entry for each type of chunk
-- in the wave file.  Read the first format chunk and disply the 
-- format information, then use the dictProcessData function
-- to enumerate over the max_iter iteratee to find the maximum value
-- (peak amplitude) in the file.
writer :: FilePath -> Maybe (IM.IntMap [WAVEDE]) -> Iteratee (V.Vector Word8) AudioMonad (Iteratee (V.Vector Double) AudioMonad ())
writer _ Nothing = error "No Dictionary"
writer fp (Just dict) = do
  fmtm <- dictReadFirstFormat dict
  liftIO $ putStrLn $ show fmtm
  maybe (error "No format")
        (\fmt -> dictProcessData 0 dict $ writeWave fp fmt)
        fmtm

