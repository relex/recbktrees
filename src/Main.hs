{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Main where

import           GHC.Generics          (Generic)

import           Options.Generic

import           Control.Monad         ((>=>))
import           Control.Monad.State   (State, evalState)
import           Data.BKTree
import           Data.BKTree.Rendering
import           Data.Functor.Foldable
import           Text.EditDistance     (defaultEditCosts, levenshteinDistance)
import Data.Functor.Identity (runIdentity)

data Args w = FindSimilar (w ::: FilePath <?> "File containing list of words") (w ::: Int <?> "Distance") (w ::: String <?> "Search condition")
            | RenderTree (w ::: FilePath <?> "File containing list of words")
            | RenderQueryTree (w ::: FilePath <?> "File containing list of words") (w ::: Int <?> "Distance") (w ::: String <?> "Search condition")
          deriving (Generic)

instance ParseRecord (Args Wrapped)

editDistance :: Distance String
editDistance = levenshteinDistance defaultEditCosts

-- | Build a tree from file
--
-- The file should contain a newline separated list of words
buildTreeFromFile :: FilePath -> IO (BKTree String)
buildTreeFromFile path = foldr (insert editDistance) empty . lines <$> readFile path

-- | Finish up the rendering
--
-- Evaluates the state monad and wraps the digraph
render :: State Int (x, String) -> String
render f = "digraph g {\n" <> snd (evalState f 0) <> "\n}"

main :: IO ()
main =
  unwrapRecord "recbktree" >>= \case
    FindSimilar path range condition ->
      -- Returns a list of similar words
      buildTreeFromFile path >>= print . search editDistance range condition
    RenderTree path ->
      -- Prints a dot file depicting the tree
      --
      -- Remember that hylo is hylo :: FAlgebra f b -> FCOAlgebra f a -> a -> b
      -- So in our case, we're giving it the dot renderer *and* the constant annotator
      --
      -- Because our algebra is monadic, we're sequencing it for each step
      buildTreeFromFile path >>= putStrLn . render . hylo (sequence >=> dot) (constAnnotate True)
    RenderQueryTree path range condition -> do
      -- Prints a dot file depicting tree traversal
      --
      -- Remember that ghylo is ghylo distAlg distCoalg alg coalg
      --
      -- Meaning that the first two arguments define which algebras are being used
      --
      -- The recursion-schemes library provides these distributives in the form of
      -- distCata, distAna, distPara, distApo ...
      --
      -- The distCata version has the Identity functor in there, so to use our original
      -- algebra with it, we need to get rid of it by calling fmap runIdentity
      let customRefold alg = ghylo distCata distApo (alg . fmap runIdentity)
          coalgebra = searchAnnotate editDistance range condition
          algebra = sequence >=> dot
      buildTreeFromFile path >>= putStrLn . render . customRefold algebra coalgebra
