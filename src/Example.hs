{-# LANGUAGE OverloadedStrings #-}

module Example where

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
          k12 <- createEdge "Knows" n1 n2
          k23 <- createEdge "Knows" n2 n3
          k14 <- createEdge "Knows" n1 n4
          k42 <- createEdge "Knows" n4 n2
          (e35, e53) <- createEdgePair "Knows" n3 n5
          _ <- setNodeProperties [("phone", "+91 8953451233"), ("name", "user 1"), ("age", "21")] n1
          _ <- setNodeProperties [("phone", "+91 8295121233"), ("name", "user 2"), ("age", "32")] n2
          _ <- setNodeProperties [("phone", "+91 8912123213"), ("name", "user 3"), ("age", "19")] n3
          _ <- setNodeProperties [("phone", "+91 7825262391"), ("name", "user 4"), ("age", "42")] n4
          _ <- setNodeProperties [("phone", "+91 9199273542"), ("name", "user 5"), ("age", "14")] n5
          _ <- setEdgeProperties [("name", "nick 12"), ("weight", "42")] k12
          _ <- setEdgeProperties [("name", "nick 23"), ("weight", "12")] k23
          _ <- setEdgeProperties [("name", "nick 14"), ("weight", "78")] k14
          _ <- setEdgeProperties [("name", "nick 42"), ("weight", "1")] k42
          _ <- setEdgeProperties [("name", "nick 35"), ("weight", "44")] e35
          _ <- setEdgeProperties [("name", "nick 53"), ("weight", "16")] e53
          c1 <- createNodeWithLabel "Activity"
          c2 <- createNodeWithLabel "Activity"
          c3 <- createNodeWithLabel "Activity"
          c4 <- createNodeWithLabel "Activity"
          c5 <- createNodeWithLabel "Activity"
          c6 <- createNodeWithLabel "Activity"
          c7 <- createNodeWithLabel "Activity"
          c8 <- createNodeWithLabel "Activity"
          _ <- setNodeProperties [("index", "1"), ("name", "football"), ("popularity", "112")] c1
          _ <- setNodeProperties [("index", "2"), ("name", "programming"), ("popularity", "19")] c2
          _ <- setNodeProperties [("index", "3"), ("name", "chess"), ("popularity", "5")] c3
          _ <- setNodeProperties [("index", "4"), ("name", "video games"), ("popularity", "66")] c4
          _ <- setNodeProperties [("index", "5"), ("name", "wrestling"), ("popularity", "31")] c5
          _ <- setNodeProperties [("index", "6"), ("name", "reading"), ("popularity", "17")] c6
          _ <- setNodeProperties [("index", "7"), ("name", "cricket"), ("popularity", "56")] c7
          _ <- setNodeProperties [("index", "8"), ("name", "tennis"), ("popularity", "12")] c8
          l11 <- createEdge "Likes" n1 c1
          l12 <- createEdge "Likes" n1 c2
          l14 <- createEdge "Likes" n1 c4
          l27 <- createEdge "Likes" n2 c7
          l25 <- createEdge "Likes" n2 c5
          l33 <- createEdge "Likes" n3 c3
          l36 <- createEdge "Likes" n3 c6
          l32 <- createEdge "Likes" n3 c2
          l48 <- createEdge "Likes" n4 c8
          l43 <- createEdge "Likes" n4 c3
          l51 <- createEdge "Likes" n5 c1
          l56 <- createEdge "Likes" n5 c6
          _ <- setEdgeProperties [("weight", "56")] l11
          _ <- setEdgeProperties [("weight", "78")] l12
          _ <- setEdgeProperties [("weight", "44")] l14
          _ <- setEdgeProperties [("weight", "5")] l27
          _ <- setEdgeProperties [("weight", "17")] l25
          _ <- setEdgeProperties [("weight", "51")] l33
          _ <- setEdgeProperties [("weight", "54")] l36
          _ <- setEdgeProperties [("weight", "24")] l32
          _ <- setEdgeProperties [("weight", "37")] l48
          _ <- setEdgeProperties [("weight", "18")] l43
          _ <- setEdgeProperties [("weight", "61")] l51
          _ <- setEdgeProperties [("weight", "8")] l56
          return ()
