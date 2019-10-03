{-# LANGUAGE LambdaCase #-}
module Data.BKTree.Rendering
  ( dot
  , constAnnotate
  , searchAnnotate
  ) where

import           Control.Comonad.Cofree       (Cofree)
import           Control.Comonad.Trans.Cofree (CofreeF (..))
import           Control.Monad.State          (State, get, modify)
import           Data.BKTree
import           Data.Bool                    (bool)
import           Data.Functor.Foldable        (ana)
import           Text.Printf                  (printf)

-- | Return a new index
newIdx :: State Int Int
newIdx = modify succ >> get

-- | Render an annotated tree into a graphviz document
--
-- We have the fixpoint type of the BKTree
--
-- BKTreeF a f = EmptyF | NodeF a [Index f]
--
-- And annotations on top of it with Cofree
--
-- CofreeF f a b = a :< (f b)
--
--
-- So a single layer could look like for example
--
-- True :< (NodeF "foo" [Index 1 (Just "foo", "foo")])
--
-- The Maybe String represents the node name of the child
-- And the String represents the graphviz doc by that time
dot :: CofreeF (BKTreeF String) Bool (Maybe String, String) -> State Int (Maybe String, String)
dot = \case
  _ :< EmptyF ->
    -- No tree, no output
    pure (Nothing, "")
  visible :< NodeF current children -> do
    -- For each node, create a node, color it based on visibility and catenate with edges and children
    -- Remember that the type of children is
    -- [Index (Maybe String, String)]
    key <- nodeKey
    let currentNode = key <> mkLabel current visible
        childNodes =
          [ mkEdge key childKey dist <> child -- concat edges and child documents
          | Index dist (Just childKey, child) <- children ]
    pure (Just key, currentNode <> concat childNodes )
  where
    mkColor visible = if not visible then "color=\"grey\"" else ""
    mkEdge :: String -> String -> Int -> String
    mkEdge = printf "%s -> %s [label=\"%d\"];\n"
    mkLabel :: String -> Bool -> String
    mkLabel label = printf " [label=\"%s\" %s];\n" label . mkColor
    nodeKey :: State Int String
    nodeKey = printf "node_%d" <$> newIdx

-- | Annotate a tree with a constant value
--
-- Remember that for coalgebras the arrows are different
--
-- algebra: f a -> a
-- coalgebra: f a <- a (or in haskell parlance a -> f a)
--
-- So in this case we're going from the normal tree into the fixpoint tree
-- annotating with the cofree with each step
constAnnotate :: x -> BKTree a -> CofreeF (BKTreeF a) x (BKTree a)
constAnnotate x = \case
  Empty -> x :< EmptyF
  Node current children -> x :< NodeF current children

-- | Annotate a tree with the result of the culling
--
-- The type signature of this one is a little bit more complex
--
-- Remember that apo is
-- a -> f (Either (f a) a)
--
-- This means that Right gives the next steps to recurse
-- and Left gives the entire subtree and stops recursion
searchAnnotate
  :: Distance a
  -> Int
  -> a
  -> BKTree a
  -> CofreeF (BKTreeF a) Bool (Either (Cofree (BKTreeF a) Bool) (BKTree a))
searchAnnotate distance range target = \case
  Empty ->
    -- Empty tree is by default false
    False :< EmptyF
  Node current children ->
    let currentDistance = distance current target
        -- The rules for accepting current node and children
        -- should be the same as when we're actually querying
        upperBound = currentDistance + range
        lowerBound = currentDistance - range
        within d = d >= lowerBound && d <= upperBound
        accepted = within currentDistance
        -- The mkFalse function takes a child (which is a subtree) and annotates
        -- the subtree with a constant False. The entire subtree has been culled by this
        mkFalse = ana (constAnnotate False)
        -- Left for entire subtree, Right for next steps
        mkChild child = bool (Left (mkFalse child)) (Right child)
        culledChildren =
          [ Index d (mkChild child (within d))
          | Index d child <- children ]
    in accepted :< NodeF current culledChildren
