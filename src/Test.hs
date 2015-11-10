{-# LANGUAGE OverloadedStrings #-}

module Test where

import           Control.Monad.State
import qualified Data.Text           as T
import           HGraph.Database
import           HGraph.Edge
import           HGraph.Graph
import           HGraph.GraphConfig
import           HGraph.Label
import           HGraph.Node
import           HGraph.Types

exec :: GS ()
exec = do n1 <- createNodeWithLabel "User"
          n2 <- createNodeWithLabel "User"
          n3 <- createNodeWithLabel "User"
          n4 <- createNodeWithLabel "User"
          n5 <- createNodeWithLabel "User"
          e12 <- createEdge "Knows" n1 n2
          e23 <- createEdge "Knows" n1 n3
          e14 <- createEdge "Knows" n1 n4
          (e35, e53) <- createEdgePair "Knows" n3 n5
          _ <- setNodeProperty "name" "user 1" n1
          _ <- setNodeProperty "name" "user 2" n2
          _ <- setNodeProperty "name" "user 3" n3
          _ <- setNodeProperty "name" "user 4" n4
          _ <- setNodeProperty "name" "user 5" n5
          _ <- setEdgeProperty "weight" "3" e12
          _ <- setEdgeProperty "weight" "2" e23
          _ <- setEdgeProperty "weight" "7" e14
          _ <- setEdgeProperty "weight" "2" e35
          _ <- setEdgeProperty "weight" "5" e53
          return ()
