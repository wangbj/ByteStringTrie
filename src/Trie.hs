{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

module Trie (
    Trie
  , empty
  , insert
  , singleton
  , assocs
  , keys
  , elems
  , insertWith
  , fromList
  , fromListWith
  , member
  , notMember
  , size
  , null
  , lookup
  ) where

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Foldable as F
import qualified Data.Sequence as Seq
import Data.Sequence(Seq)
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict(IntMap)
import Data.ByteString.Builder
import Control.Arrow
import Control.Monad.Writer
import Data.Monoid
import Data.Char
import Prelude hiding (null, lookup, elems)

newtype Trie a = Trie {
  unTrie :: IntMap (Maybe a, Trie a)
  } 

empty = Trie IntMap.empty

member (C.uncons -> Nothing) _ = True
member (C.uncons -> Just (c, cs)) (Trie t) = case IntMap.lookup (ord c) t of
  Nothing -> False
  Just (_, tr) -> member cs tr
notMember s = not . member s

lookup bs = go Nothing bs
  where go r s (Trie t) = case C.uncons s of
          Nothing -> r
          Just (c, cs) -> case IntMap.lookup (ord c) t of
            Nothing -> Nothing
            Just (v, tr) -> go v cs tr

insertWith f (C.uncons -> Nothing) v t = t
insertWith f (C.uncons -> Just (c, cs)) v (Trie t)
  | C.null cs = case IntMap.lookup k t of
    Nothing -> Trie $ IntMap.insert k (Just v, empty) t
    Just _ -> Trie $ IntMap.update (Just . first (\x -> case x of
                                                      Nothing -> Just v
                                                      Just x' -> Just (f v x'))) k t
  | otherwise = case IntMap.lookup k t of
    Nothing -> Trie $ IntMap.insert k (Nothing, insertWith f cs v empty) t
    Just _ -> Trie $ IntMap.update (Just . fmap (insertWith f cs v) ) k t
  where k = ord c
          

insert s v = insertWith const s v
singleton k v = insertWith const k v empty

fromListWith :: (a -> a -> a) -> [ (C.ByteString, a) ] -> Trie a
fromListWith f = foldl acc empty
  where acc t (k, v) = insertWith f k v t

fromList = fromListWith const 

cnt m t = foldMap (cntM m) (IntMap.elems (unTrie t))
cntM m (Nothing, t) = cnt m t
cntM m (Just v, t) = Sum 1

size = getSum . cnt (Sum 0)

null = IntMap.null . unTrie

cons m t = mapM_ (uncurry (consM m)) (IntMap.assocs (unTrie t))
consM m k (Nothing, t) = cons (m<>char8 (chr k)) t
consM m k (Just v, t) = tell (Seq.singleton ((L.toStrict . toLazyByteString) m', v)) >> cons m' t
  where m' = m <> char8 (chr k)

assocs t = F.toList $ execWriter (cons mempty t)
keys = fmap fst . assocs
elems = fmap snd . assocs

instance (Show a) => Show (Trie a) where
  show t = show $ execWriter (cons mempty t)
