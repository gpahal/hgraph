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
exec = do n1 <- createNodeWithLabelRN "User"
          n2 <- createNodeWithLabelRN "User"
          n3 <- createNodeWithLabelRN "User"
          n4 <- createNodeWithLabelRN "User"
          n5 <- createNodeWithLabelRN "User"
          (e12, n11, n21) <- createEdgeN "Knows" n1 n2
          (e23, n22, n31) <- createEdgeN "Knows" n21 n3
          (e14, n12, n41) <- createEdgeN "Knows" n11 n4
          (e35, e53, n32, n51) <- createEdgeNPair "Knows" n31 n5
          n13 <- setNodePropertyN "name" "user 1" n12
          n23 <- setNodePropertyN "name" "user 2" n22
          n33 <- setNodePropertyN "name" "user 3" n32
          n42 <- setNodePropertyN "name" "user 4" n41
          n52 <- setNodePropertyN "name" "user 5" n51
          e121 <- setEdgePropertyE "weight" "3" e12
          e231 <- setEdgePropertyE "weight" "2" e23
          e141 <- setEdgePropertyE "weight" "7" e14
          e351 <- setEdgePropertyE "weight" "2" e35
          e531 <- setEdgePropertyE "weight" "5" e53
          return ()
