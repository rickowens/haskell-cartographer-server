{-# LANGUAGE OverloadedStrings #-}
{- |
  Template haskell tools for compiling and incliding Elm files.
-}
module Network.Eureka.Cartographer.TH (
  readDot
) where

import Data.GraphViz (parseDotGraph, DotGraph)
import Data.Text.Lazy (Text, pack)
import Language.Haskell.TH (Exp(LitE), Lit(StringL), Q, runIO)

readDot :: Q Exp
readDot = runIO $ do
  dotStr <- readFile "cartography.dot"
  let parsedGraph :: DotGraph Text
      parsedGraph = parseDotGraph (pack dotStr)
  -- | verify that the dot graph parses without error
  parsedGraph `seq` (return . LitE . StringL) dotStr
  where

