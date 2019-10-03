{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
module Data.BKTree
  ( BKTree(..)
  , BKTreeF(..)
  , Index(..)
  , Distance
  , insert
  , search
  , empty
  ) where

import           Data.Functor.Foldable    (cata)
import           Data.Functor.Foldable.TH

-- | Represents a distance from the 'current node' to a child node
data Index a = Index !Int a deriving (Functor, Foldable, Traversable)


-- | The base type of a bk-tree
data BKTree a = Empty
              | Node !a [Index (BKTree a)]
              deriving (Functor, Foldable, Traversable)

-- This TH creates a new data fixpoint type like
-- data BKTreeF a f = Empty | NodeF a [Index f]
--
-- and the associated type families and type class instances
makeBaseFunctor ''BKTree


-- | Represents a distance between two objects
--
-- This needs to be a metric space
--
-- a == a, dist(a,a) = 0
-- a /= b, dist(a, b) > 0
-- dist(a,b) == dist(b,a)
-- dist(a,c) < dist(a+b) + dist(b,c)
type Distance a = a -> a -> Int

empty :: BKTree a
empty = Empty

-- | Insert a node to a tree
insert :: Distance a -> a -> BKTree a -> BKTree a
insert distance a = \case
  Empty -> Node a []
  Node b children ->
    let newDistance = distance a b
    in Node b (addChild newDistance children)
  where
    addChild d = \case
      [] -> [Index d (insert distance a Empty)]
      Index d' child:children | d == d' -> Index d' (insert distance a child) : children
                              | otherwise -> Index d' child : addChild d children

-- | Search elements within range
search :: forall a. Distance a -> Int -> a -> BKTree a -> [a]
search distance range target = cata searchF
  where
    -- The algebra for finding nearby elements
    searchF :: BKTreeF a [a] -> [a]
    searchF = \case
      EmptyF -> [] -- No results from empty tree
      NodeF current children ->
        -- Remember that NodeF is NodeF a [Index f]
        -- Which in this case would be NodeF a [Index [a]]
        -- where the [a] has the semantics of included children
        let currentDistance = distance current target
            upperBound = currentDistance + range
            lowerBound = currentDistance - range
            -- Since the children already contain the accepted children
            -- from earlier recursion, just concatenate the ones we
            -- accept on this layer
            includedChildren =
              concat [ xs
                     | Index dist xs <- children
                     , dist <= upperBound, dist >= lowerBound ]
        in if currentDistance <= range
              then current : includedChildren
              else includedChildren
