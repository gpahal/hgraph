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
          (e12, n11, n21) <- createEdge "Knows" n1 n2
          (e23, n22, n31) <- createEdge "Knows" n21 n3
          (e14, n12, n41) <- createEdge "Knows" n11 n4
          (e35, e53, n32, n51) <- createEdgePair "Knows" n31 n5
          n13 <- setNodeProperty "name" "user 1" n12
          n23 <- setNodeProperty "name" "user 2" n22
          n33 <- setNodeProperty "name" "user 3" n32
          n42 <- setNodeProperty "name" "user 4" n41
          n52 <- setNodeProperty "name" "user 5" n51
          e121 <- setEdgeProperty "weight" "3" e12
          e231 <- setEdgeProperty "weight" "2" e23
          e141 <- setEdgeProperty "weight" "7" e14
          e351 <- setEdgeProperty "weight" "2" e35
          e531 <- setEdgeProperty "weight" "5" e53
          return ()
