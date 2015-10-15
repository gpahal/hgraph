module HGraph.Node where

import           Control.Monad.State
import qualified Data.Map            as M
import           Data.Maybe
import qualified Data.Set            as S
import           HGraph.Graph
import           HGraph.Label
import           HGraph.Types

emptyNode :: Id -> Node
emptyNode i = Node S.empty i M.empty M.empty M.empty

saveNode :: Node -> GS Node
saveNode n = do g <- get
                alterNodes (M.insert (nodeId n) n $ nodes g)
                return n


alterNodeLabelIndices :: Node -> S.Set LabelIndex -> Node
alterNodeLabelIndices n ls = Node ls (nodeId n) (nodeProperties n) (outEdges n) (inEdges n)

alterNodeProperties :: Node -> Properties -> Node
alterNodeProperties n p = Node (nodeLabelIndices n) (nodeId n) p (outEdges n) (inEdges n)

alterOutEdges :: Node -> Neighbors -> Node
alterOutEdges n oe = Node (nodeLabelIndices n) (nodeId n) (nodeProperties n) oe (inEdges n)

alterInEdges :: Node -> Neighbors -> Node
alterInEdges n = Node (nodeLabelIndices n) (nodeId n) (nodeProperties n) (outEdges n)

addOutEdge :: Node -> Edge -> Node -> LabelIndex -> Node
addOutEdge n e en li = alterOutEdges n nbs
    where
        oe  = outEdges n
        s   = fromMaybe M.empty $ M.lookup li oe
        nbs = M.insert li (M.insert (edgeId e) (nodeId en) s) oe

addInEdge :: Node -> Edge -> Node -> LabelIndex -> Node
addInEdge n e sn li = alterInEdges n nbs
    where
        ie  = inEdges n
        s   = fromMaybe M.empty $ M.lookup li ie
        nbs = M.insert li (M.insert (edgeId e) (nodeId sn) s) ie


addLabelToNode :: Label -> Node -> GS Node
addLabelToNode l n = do nli <- createNodeLabel l
                        saveNode $ alterNodeLabelIndices n (S.insert nli $ nodeLabelIndices n)

addLabelsToNode :: S.Set Label -> Node -> GS Node
addLabelsToNode ls n = do nlis <- createNodeLabels ls
                          saveNode $ alterNodeLabelIndices n (S.union nlis $ nodeLabelIndices n)


createNode :: GS Node
createNode = do i <- incrementNodeId
                saveNode $ emptyNode i

createNodeWithLabel :: Label -> GS Node
createNodeWithLabel l = createNode >>= addLabelToNode l

createNodeWithLabels :: S.Set Label -> GS Node
createNodeWithLabels ls = createNode >>= addLabelsToNode ls


setNodeProperty :: Key -> Value -> Node -> GS Node
setNodeProperty k v n = saveNode newNode
    where
        newNode  = alterNodeProperties n $ M.insert k v $ nodeProperties n

getNodeProperty :: Key -> Node -> Maybe Value
getNodeProperty k n = M.lookup k (nodeProperties n)
