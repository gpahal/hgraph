{-# LANGUAGE FlexibleInstances #-}

module HGraph.Types where

import           Control.Applicative
import           Control.Monad.State
import           Data.Int
import qualified Data.Map            as M
import qualified Data.Set            as S
import qualified Data.Text           as T

type Key = T.Text

data Value = VInt Int64
           | VBool Bool
           | VDouble Double
           | VText T.Text
           | VIntList [Int64]
           | VBoolList [Bool]
           | VDoubleList [Double]
           | VTextList [T.Text]
           deriving (Eq, Show)

data Type = NodeType
          | EdgeType
          deriving (Eq, Show)

type Label = T.Text

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
            deriving (Eq, Show)

data PathTree = PathTree Node [(Edge, PathTree)]
                deriving (Eq, Show)

idKey :: String
idKey = "hgraph_id"

idTextKey :: T.Text
idTextKey = T.pack idKey

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


class TextValue a where
    toText :: a -> Key

instance TextValue String where
    toText = T.pack

instance TextValue T.Text where
    toText = id

class IntValue a where
    toInt :: a -> Int64
    fromInt :: Int64 -> a

instance IntValue Int where
    toInt = fromInteger . toInteger
    fromInt = fromInteger . toInteger

instance IntValue Int32 where
    toInt = fromInteger . toInteger
    fromInt = fromInteger . toInteger

instance IntValue Int64 where
    toInt = fromInteger . toInteger
    fromInt = fromInteger . toInteger

class PropertyValue a where
    toValue :: a -> Value

instance PropertyValue Int where
    toValue = VInt . toInt

instance PropertyValue Int32 where
    toValue = VInt . toInt

instance PropertyValue Int64 where
    toValue = VInt . toInt

instance PropertyValue Bool where
    toValue = VBool

instance PropertyValue Double where
    toValue = VDouble

instance PropertyValue String where
    toValue = VText . T.pack

instance PropertyValue T.Text where
    toValue = VText

instance PropertyValue [Int] where
    toValue = VIntList . map (fromInteger . toInteger)

instance PropertyValue [Int32] where
    toValue = VIntList . map (fromInteger . toInteger)

instance PropertyValue [Int64] where
    toValue = VIntList . map (fromInteger . toInteger)

instance PropertyValue [Bool] where
    toValue = VBoolList

instance PropertyValue [Double] where
    toValue = VDoubleList

instance PropertyValue [String] where
    toValue = VTextList . map T.pack

instance PropertyValue [T.Text] where
    toValue = VTextList

instance PropertyValue Value where
    toValue = id
