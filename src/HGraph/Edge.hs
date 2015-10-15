module HGraph.Edge where

import           Control.Monad.State
import qualified Data.Map            as M
import           HGraph.Graph
import           HGraph.Label
import           HGraph.Node
import           HGraph.Types

emptyEdge :: LabelIndex -> Id -> Node -> Node -> Edge
emptyEdge li i sn en = Edge li i M.empty (nodeId sn, nodeId en)


saveEdge :: Edge -> GS Edge
saveEdge e = do g <- get
                alterEdges (M.insert (edgeId e) e $ edges g)
                return e


alterEdgeLabelIndex :: LabelIndex -> Edge -> Edge
alterEdgeLabelIndex li e = Edge li (edgeId e) (edgeProperties e) (connection e)

alterEdgeProperties :: Properties -> Edge -> Edge
alterEdgeProperties p e = Edge (edgeLabelIndex e) (edgeId e) p (connection e)


createEdge :: Node -> Node -> Label -> GS Edge
createEdge sn en l = do i <- incrementEdgeId
                        eli <- createEdgeLabel l
                        let e = emptyEdge eli i sn en
                        _ <- saveNode $ addOutEdge e en eli sn
                        _ <- saveNode $ addInEdge e sn eli en
                        saveEdge e


setEdgeProperty :: Key -> Value -> Edge -> GS Edge
setEdgeProperty k v e = saveEdge newEdge
    where
        newEdge  = alterEdgeProperties (M.insert k v $ edgeProperties e) e

getEdgeProperty :: Key -> Edge -> Maybe Value
getEdgeProperty k e = M.lookup k (edgeProperties e)

getEdgePropertyM :: Key -> Edge -> GS (Maybe Value)
getEdgePropertyM k e = return $ getEdgeProperty k e

isEdgePropertyEqual :: Key -> Value -> Edge -> Bool
isEdgePropertyEqual k v e = maybe False (==v) $ getEdgeProperty k e

isEdgePropertyEqualM :: Key -> Value -> Edge -> GS Bool
isEdgePropertyEqualM k v e = return $ isEdgePropertyEqual k v e

hasEdgeLabelIndex :: LabelIndex -> Edge -> Bool
hasEdgeLabelIndex li e = edgeLabelIndex e == li

hasEdgeLabelIndexM :: LabelIndex -> Edge -> GS Bool
hasEdgeLabelIndexM li e = return $ hasEdgeLabelIndex li e
