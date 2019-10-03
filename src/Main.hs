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

buildTreeFromFile :: FilePath -> IO (BKTree String)
buildTreeFromFile path = foldr (insert editDistance) empty . lines <$> readFile path

render :: State Int (x, String) -> String
render f = "digraph g {\n" <> snd (evalState f 0) <> "\n}"

main :: IO ()
main =
  unwrapRecord "recbktree" >>= \case
    FindSimilar path range condition ->
      buildTreeFromFile path >>= print . search editDistance range condition
    RenderTree path ->
      buildTreeFromFile path >>= putStrLn . render . hylo (sequence >=> dot) (constAnnotate True)
    RenderQueryTree path range condition -> do
      let customRefold alg = ghylo distCata distApo (alg . fmap runIdentity)
          coalgebra = searchAnnotate editDistance range condition
          algebra = sequence >=> dot
      buildTreeFromFile path >>= putStrLn . render . customRefold algebra coalgebra
