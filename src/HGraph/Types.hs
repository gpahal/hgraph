{-# LANGUAGE ExistentialQuantification #-}
-- existential quantification to be used for the value type

module HGraph.Types where

import           Control.Applicative
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
                 } deriving (Show)

data Edge = Edge { edgeLabelIndex :: LabelIndex
                 , edgeId         :: Id
                 , edgeProperties :: Properties
                 , connection     :: Connection
                 } deriving (Show)

instance Eq Node where
    a == b = nodeId a == nodeId b

instance Eq Edge where
    a == b = edgeId a == edgeId b

data Direction = DIN
               | DOUT
               | DBOTH
               deriving (Eq, Show)

type LabelIndexMap = M.Map LabelIndex Label

type LabelIndexMaps = (LabelIndexMap, LabelIndexMap)

type LabelMap = M.Map Label LabelIndex

type LabelMaps = (LabelMap, LabelMap)

type LabelInstances = M.Map LabelIndex (S.Set Id)

type Nodes = M.Map Id Node

type Edges = M.Map Id Edge

data GraphConfig = GraphConfig { nextNodeLabelIndex :: LabelIndex
                               , nextEdgeLabelIndex :: LabelIndex
                               , nextNodeId         :: Id
                               , nextEdgeId         :: Id
                               } deriving (Eq, Show)

data Graph = Graph { graphConfig        :: GraphConfig
                   , labelIndexMaps     :: LabelIndexMaps
                   , labelMaps          :: LabelMaps
                   , nodeLabelInstances :: LabelInstances
                   , nodes              :: Nodes
                   , edges              :: Edges
                   } deriving (Eq, Show)

type GS a = State Graph a

data Path = Path Node [(Edge, Node)]

data PathTree = PathTree Node [(Edge, PathTree)]

startId :: Id
startId = 1

startLabelIndex :: LabelIndex
startLabelIndex = 1

unpackStateValue :: (a -> GS b) -> Graph -> a -> b
unpackStateValue f g a = evalState (f a) g

mapStateValue :: (a -> b) -> GS a -> GS b
mapStateValue f = mapState nf
    where
        nf (x, y) = (f x, y)

combineGS :: (a -> b -> c) -> GS a -> GS b -> GS c
combineGS f gs1 gs2 = f <$> gs1 <*> gs2

