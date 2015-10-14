module HGraph.Node where

import HGraph.Types
import HGraph.Graph
import HGraph.Label
import qualified Data.Map as Map
import qualified Data.Set as Set

emptyNode :: Id -> Node
emptyNode i = Node Set.empty i Map.empty Map.empty Map.empty

saveNode :: Graph -> Node -> Graph
saveNode g n = alterNodes g (Map.insert (nodeId n) n $ nodes g)

alterLabelIndices :: Node -> Set.Set LabelIndex -> Node
alterLabelIndices n ls = Node ls (nodeId n) (nodeProperties n) (outEdges n) (inEdges n)

alterNodeProperties :: Node -> Properties -> Node
alterNodeProperties n p = Node (nodeLabelIndices n) (nodeId n) p (outEdges n) (inEdges n)

alterOutEdges :: Node -> Neighbors -> Node
alterOutEdges n oe = Node (nodeLabelIndices n) (nodeId n) (nodeProperties n) oe (inEdges n)

alterInEdges :: Node -> Neighbors -> Node
alterInEdges n = Node (nodeLabelIndices n) (nodeId n) (nodeProperties n) (outEdges n)


addLabelToNode :: Graph -> Node -> Label -> (Graph, Node)
addLabelToNode g n l = (alterNodes newGraph (Map.insert i newNode $ nodes g), newNode)
    where
        i                         = nodeId n
        (newGraph, newLabelIndex) = createNodeLabel g l
        newNode                   = alterLabelIndices n (Set.insert newLabelIndex $ nodeLabelIndices n)

addLabelsToNode :: Graph -> Node -> Set.Set Label -> (Graph, Node)
addLabelsToNode g n ls = (alterNodes newGraph (Map.insert i newNode $ nodes g), newNode)
    where
        i                            = nodeId n
        (newGraph, newLabelIndexSet) = createNodeLabels g ls
        newNode                      = alterLabelIndices n (Set.union newLabelIndexSet $ nodeLabelIndices n)


createNode :: Graph -> (Graph, Node)
createNode g = (incrementNodeId $ alterNodes g (Map.insert nextId newNode $ nodes g), newNode)
    where
        nextId  = nextNodeId $ graphConfig g
        newNode = emptyNode nextId

createNodeWithLabel :: Graph -> Label -> (Graph, Node)
createNodeWithLabel g = addLabelToNode newGraph newNode
    where
        (newGraph, newNode) = createNode g

createNodeWithLabels :: Graph -> Set.Set Label -> (Graph, Node)
createNodeWithLabels g = addLabelsToNode newGraph newNode
    where
        (newGraph, newNode) = createNode g


getNodeProperty :: Node -> Key -> Maybe Value
getNodeProperty n k = Map.lookup k (nodeProperties n)

setNodeProperty :: Graph -> Node -> Key -> Value -> (Graph, Node)
setNodeProperty g n k v = (newGraph, newNode)
    where
        newNode  = alterNodeProperties n (Map.insert k v $ nodeProperties n)
        newGraph = saveNode g newNode