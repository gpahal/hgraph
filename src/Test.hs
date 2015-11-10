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
exec = do n1 <- createNodeWithLabel $ T.pack "User"
          n2 <- createNodeWithLabel $ T.pack "User"
          n3 <- createNodeWithLabel $ T.pack "User"
          n4 <- createNodeWithLabel $ T.pack "User"
          n5 <- createNodeWithLabel $ T.pack "User"
          (e12, n11, n21) <- createEdge (T.pack "Knows") n1 n2
          (e23, n22, n31) <- createEdge (T.pack "Knows") n21 n3
          (e14, n12, n41) <- createEdge (T.pack "Knows") n11 n4
          n13 <- setNodeProperty (T.pack "name") (T.pack "user 1") n12
          n23 <- setNodeProperty (T.pack "name") (T.pack "user 2") n22
          n32 <- setNodeProperty (T.pack "name") (T.pack "user 3") n31
          n42 <- setNodeProperty (T.pack "name") (T.pack "user 4") n41
          e121 <- setEdgeProperty (T.pack "weight") (T.pack "3") e12
          e231 <- setEdgeProperty (T.pack "weight") (T.pack "2") e23
          e141 <- setEdgeProperty (T.pack "weight") (T.pack "7") e14
          return ()
