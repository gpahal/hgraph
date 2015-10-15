{-# LANGUAGE ExistentialQuantification #-}
-- existential quantification to be used for the value type

module HGraph.Types where

import           Control.Monad.State
import           Data.Int
import qualified Data.Map            as M
import qualified Data.Set            as S

type Key = String

-- to be updated
type Value = String

data Type = NodeType
          | EdgeType
          deriving (Eq, Show)

type Label = String

type LabelIndex = Int32

type Id = Int64

type LabelNeighbors = M.Map Id Id

type Neighbors = M.Map LabelIndex LabelNeighbors

type Connection = (Id, Id)

type Properties = M.Map Key Value

data Node = Node { nodeLabelIndices :: S.Set LabelIndex
                 , nodeId           :: Id
                 , nodeProperties   :: Properties
                 , outEdges         :: Neighbors
                 , inEdges          :: Neighbors
                 } deriving (Eq, Show)

data Edge = Edge { edgeLabelIndex :: LabelIndex
                 , edgeId         :: Id
                 , edgeProperties :: Properties
                 , connection     :: Connection
                 } deriving (Eq, Show)

type LabelIndexMap = M.Map LabelIndex Label

type LabelIndexMaps = (LabelIndexMap, LabelIndexMap)

type LabelMap = M.Map Label LabelIndex

type LabelMaps = (LabelMap, LabelMap)

type LabelInstance = M.Map LabelIndex (S.Set Id)

type LabelInstances = (LabelInstance, LabelInstance)

type Nodes = M.Map Id Node

type Edges = M.Map Id Edge

data GraphConfig = GraphConfig { nextNodeLabelIndex :: LabelIndex
                               , nextEdgeLabelIndex :: LabelIndex
                               , nextNodeId         :: Id
                               , nextEdgeId         :: Id
                               } deriving (Eq, Show)

data Graph = Graph { graphConfig    :: GraphConfig
                   , labelIndexMaps :: LabelIndexMaps
                   , labelMaps      :: LabelMaps
                   , labelInstances :: LabelInstances
                   , nodes          :: Nodes
                   , edges          :: Edges
                   } deriving (Eq, Show)

type GS a = State Graph a
