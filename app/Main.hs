{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Char8 as C
import qualified Data.Map as Map
import Data.Map(Map)
import qualified Trie as Trie
import Trie(Trie)

ex1 :: [ (C.ByteString, Int) ]
ex1 = [ ("12345", 1), ("12345", 1), ("12354", 1), ("13245", 1) ]

fromList :: (Integral a) => [ (C.ByteString, a) ] -> Trie a
fromList = foldl acc Trie.empty
  where acc t (k, v) = Trie.insertWith (+) k v t

main :: IO ()
main = return ()
