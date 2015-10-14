{-# LANGUAGE ExistentialQuantification #-}
-- existential quantification to be used for the value type

module HGraph.Types where

import Data.Int
import Data.Set
import Data.Map

type Key = String

-- to be updated
type Value = String

data Type = NodeType
          | EdgeType
          deriving (Eq, Show)

type Label = String

type LabelIndex = Int32

type Id = Int64

type LabelNeighbors = Map Id Id

type Neighbors = Map LabelIndex LabelNeighbors

type Connection = (Id, Id)

type Properties = Map Key Value

data Node = Node { nodeLabelIndices :: Set LabelIndex
                 , nodeId :: Id
                 , nodeProperties :: Properties
                 , outEdges :: Neighbors
                 , inEdges :: Neighbors
                 } deriving (Eq, Show)

data Edge = Edge { edgeLabelIndices :: Set LabelIndex
                 , edgeId :: Id
                 , edgeProperties :: Properties
                 , connecion :: Connection
                 } deriving (Eq, Show)

type LabelIndexMap = Map LabelIndex Label

type LabelIndexMaps = (LabelIndexMap, LabelIndexMap)

type LabelMap = Map Label LabelIndex

type LabelMaps = (LabelMap, LabelMap)

type LabelInstance = Map LabelIndex (Set Id)

type LabelInstances = (LabelInstance, LabelInstance)

type Nodes = Map Id Node

type Edges = Map Id Edge

data GraphConfig = GraphConfig { nextNodeLabelIndex :: LabelIndex
                               , nextEdgeLabelIndex :: LabelIndex
                               , nextNodeId :: Id
                               , nextEdgeId :: Id
                               } deriving (Eq, Show)

data Graph = Graph { graphConfig :: GraphConfig
                   , labelIndexMaps :: LabelIndexMaps
                   , labelMaps :: LabelMaps
                   , labelInstances :: LabelInstances
                   , nodes :: Nodes
                   , edges :: Edges
                   } deriving (Eq, Show)
