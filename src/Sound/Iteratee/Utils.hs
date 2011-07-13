module Sound.Iteratee.Utils (
  interleave
)

where

import qualified Data.Vector.Storable as V
import           Foreign.Storable

-- | interleave multiple vectors.  The inputs must all have the same length,
-- else Nothing.
-- 
-- if there are no input vectors, output @Just V.empty@
interleave :: Storable a => [V.Vector a] -> Maybe (V.Vector a)
interleave []   = Just $ V.empty
interleave vecs = if all (== l) ls
  then Just $ V.generate (nVecs * l) gFunc
  else Nothing
 where
  (l:ls)   = map V.length vecs
  nVecs    = length vecs
  gFunc ix = let (frame,vec) = divMod ix nVecs
             in (vecs !! vec) V.! frame
{-# INLINEABLE interleave #-}
  -- this is probably not the most efficient, but it'll work for now
