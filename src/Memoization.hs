module Memoization (
  memoize,
  memoBivar,
  memoize3,
  lookupBivar,
  memBivarNode,
  memTableBivarNodeInt,
  memoIntTrie,
  lookupIntTrie
  ) where

import qualified Data.MemoCombinators as Memo
import qualified Data.Map as Map
import Types
import Data.IntTrie

data DList a = DList { left :: [a], right :: [a] }

instance Functor DList where
  fmap f (DList l r) = DList (map f l) (map f r)

-- Indexing in DList
(!!!) :: DList a -> Int -> a
(!!!) (DList l r) n = if n < 0 then l !! abs n else r !! n

memo2 :: (Int -> Int -> a) -> [DList a]
memo2 f = map (\x -> fmap (f x) (DList [0..] [0..])) [0..]

memoizeWDList :: [DList a] -> Int -> Int -> a
memoizeWDList f x y = let dl = f !! x in dl !!! y

memoize :: ((r -> a) -> r -> a) -> ((r -> a) -> r -> a) -> r -> a
memoize memoF next = let mf = memoF (next mf) in mf

memo3 :: (Int -> Int -> Int -> a) -> [DList (DList a)]
memo3 f = map (\t -> fmap (\y -> fmap (\x -> f x y t) (DList [0,-1..] [0..])) (DList [0,-1..] [0..])) [0..]

lookup3 :: [DList (DList a)] -> Int -> Int -> Int -> a
lookup3 cont x y t = (cont !! t) !!! y !!! x

memoize3 = lookup3 . memo3

memoBivar :: (BivarNode -> Int -> a) -> [DList (DList a)]
memoBivar f = memo3 $ \x y t -> f (BivarNode (x,y)) t

lookupBivar :: [DList (DList a)] -> BivarNode -> Int -> a
lookupBivar cont (BivarNode (x,y)) = lookup3 cont x y

memIntPair = Memo.pair Memo.integral Memo.integral

toBivarNode :: (Int,Int) -> BivarNode
toBivarNode a = BivarNode a

memBivarNode = Memo.wrap toBivarNode unBivarNode memIntPair

memTableBivarNodeInt :: (BivarNode -> Int -> a) -> BivarNode -> Int -> a
memTableBivarNodeInt = Memo.memo2 memBivarNode Memo.integral

-- memoMap :: (BivarNode -> Int -> a) -> Map.Map (BivarNode, Int) a
-- memoMap f = fmap (\(n,i) -> f n i) $  
--             Map.fromList [(BivarNode (x,y), t) | t <- [0..], x <- [0..t], y <- [0..t]]

-- lookupMap :: Map.Map (BivarNode, Int) a -> BivarNode -> Int -> a
-- lookupMap map n i = map Map.! (n,i)

memoIntTrie :: (BivarNode -> Int -> a) -> IntTrie (IntTrie (IntTrie a))
memoIntTrie f = fmap (\t -> fmap (\y -> fmap (\x -> f (BivarNode (x,y)) t) identity) identity) identity
                       
lookupIntTrie :: IntTrie (IntTrie (IntTrie a)) -> BivarNode -> Int -> a                       
lookupIntTrie trie (BivarNode (x,y)) t = apply (apply (apply trie t) y) x
