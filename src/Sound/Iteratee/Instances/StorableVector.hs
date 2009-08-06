{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, UndecidableInstances, ScopedTypeVariables #-}

module Sound.Iteratee.Instances.StorableVector (
  vmap
)

where

import qualified Data.Iteratee.Base.StreamChunk as SC
import qualified Data.StorableVector as SV
import qualified Data.StorableVector.Base as SVBase
import Data.Monoid
import qualified Data.ListLike as LL
import Foreign.Storable
import Foreign.Marshal.Array

instance (Storable el) => LL.ListLike (SV.Vector el) el where
  length    = SV.length
  null      = SV.null
  singleton = SV.singleton
  cons      = SV.cons
  head      = SV.head
  tail      = SV.tail
  findIndex = SV.findIndex
  splitAt   = SV.splitAt
  dropWhile = SV.dropWhile
  fromList  = SV.pack
  toList    = SV.unpack

instance (Storable el) => LL.FoldableLL (SV.Vector el) el where
  foldl     = SV.foldl
  foldr     = SV.foldr

instance (Storable el, LL.ListLike (SV.Vector el) el) =>
         SC.StreamChunk SV.Vector el where
  cMap      = vmap

vmap :: (SC.StreamChunk s' el', Storable el) =>
        (el -> el') ->
        SV.Vector el ->
        s' el'
vmap f xs = step xs
  where
  step bs
    | SC.null bs = mempty
    | True       = f (SC.head bs) `SC.cons` step (SC.tail bs)

-- a specialized vmap for the RULE
vmap' :: (Storable el, Storable el') =>
         (el -> el') ->
         SV.Vector el ->
         SV.Vector el'
vmap' = SV.map

{-# RULES "svmap/map" forall s (f :: (Storable el') => el -> el'). vmap f s = vmap' f s #-}

instance forall el.(Storable el) => SC.ReadableChunk SV.Vector el where
  readFromPtr p l =
    let s = sizeOf (undefined :: el)
        l' = l `div` s
    in
    if rem l s /= 0 then error $
      "Error reading stream: invalid number of bytes: " ++ (show l) ++ " size: " ++ show s
    -- need to copy the data to a new buffer so the rest of the code can use it
    -- properly.
    else SVBase.create l' $ \newp -> copyArray newp p l'

