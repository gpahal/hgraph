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


alterNodeLabelIndices :: S.Set LabelIndex -> Node -> Node
alterNodeLabelIndices ls n = Node ls (nodeId n) (nodeProperties n) (outEdges n) (inEdges n)

alterNodeProperties :: Properties -> Node -> Node
alterNodeProperties p n = Node (nodeLabelIndices n) (nodeId n) p (outEdges n) (inEdges n)

alterOutEdges :: Neighbors -> Node -> Node
alterOutEdges oe n = Node (nodeLabelIndices n) (nodeId n) (nodeProperties n) oe (inEdges n)

alterInEdges :: Neighbors -> Node -> Node
alterInEdges ie n = Node (nodeLabelIndices n) (nodeId n) (nodeProperties n) (outEdges n) ie

addOutEdge :: Edge -> Node -> LabelIndex -> Node -> Node
addOutEdge e en li n = alterOutEdges nbs n
    where
        oe  = outEdges n
        s   = fromMaybe M.empty $ M.lookup li oe
        nbs = M.insert li (M.insert (edgeId e) (nodeId en) s) oe

addInEdge :: Edge -> Node -> LabelIndex -> Node -> Node
addInEdge e sn li n = alterInEdges nbs n
    where
        ie  = inEdges n
        s   = fromMaybe M.empty $ M.lookup li ie
        nbs = M.insert li (M.insert (edgeId e) (nodeId sn) s) ie


addLabelToNode :: Label -> Node -> GS Node
addLabelToNode l n = do g <- get
                        nli <- createNodeLabel l
                        alterAddLabelInstance (nodeLabelInstances g) nli n
                        saveNode $ alterNodeLabelIndices (S.insert nli $ nodeLabelIndices n) n


addLabelsToNode :: S.Set Label -> Node -> GS Node
addLabelsToNode ls n = do g <- get
                          nlis <- createNodeLabels ls
                          alterAddLabelInstances (nodeLabelInstances g) nlis n
                          saveNode $ alterNodeLabelIndices (S.union nlis $ nodeLabelIndices n) n


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
        newNode  = alterNodeProperties (M.insert k v $ nodeProperties n) n

getNodeProperty :: Key -> Node -> Maybe Value
getNodeProperty k n = M.lookup k (nodeProperties n)

getNodePropertyM :: Key -> Node -> GS (Maybe Value)
getNodePropertyM k n = return $ getNodeProperty k n

isNodePropertyEqual :: Key -> Value -> Node -> Bool
isNodePropertyEqual k v n = maybe False (==v) $ getNodeProperty k n

isNodePropertyEqualM :: Key -> Value -> Node -> GS Bool
isNodePropertyEqualM k v n = return $ isNodePropertyEqual k v n

hasNodeLabelIndex :: LabelIndex -> Node -> Bool
hasNodeLabelIndex li n = S.member li $ nodeLabelIndices n

hasNodeLabelIndexM :: LabelIndex -> Node -> GS Bool
hasNodeLabelIndexM li n = return $ hasNodeLabelIndex li n
