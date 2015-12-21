module Util.MultiMap (MultiMap, empty, insert, lookup) where

import qualified Data.Map as Map
import Prelude hiding (lookup)

-- Simple Multimap type

type MultiMap k e = Map.Map k [e]

empty = Map.empty

insert key value mmap = case Map.lookup key mmap of
        Nothing -> Map.insert key [value] mmap
        Just old -> Map.insert key (value:old) mmap

lookup key mmap = case Map.lookup key mmap of 
        Just values -> values
        Nothing -> []
