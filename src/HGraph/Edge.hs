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


createEdge :: Label -> Node -> Node -> GS Edge
createEdge l sn en = do i <- incrementEdgeId
                        eli <- createEdgeLabel l
                        let e = emptyEdge eli i sn en
                        _ <- saveNode $ addOutEdge e en eli sn
                        _ <- saveNode $ addInEdge e sn eli en
                        saveEdge e


setEdgeProperty :: Key -> Value -> Edge -> GS Edge
setEdgeProperty k v e = saveEdge newEdge
    where
        newEdge  = alterEdgeProperties (M.insert k v $ edgeProperties e) e

getEdgePropertyS :: Key -> Edge -> Maybe Value
getEdgePropertyS k e = M.lookup k (edgeProperties e)

getEdgeProperty :: Key -> Edge -> GS (Maybe Value)
getEdgeProperty k e = return $ getEdgePropertyS k e

isEdgePropertyEqualS :: Key -> Value -> Edge -> Bool
isEdgePropertyEqualS k v e = maybe False (==v) $ getEdgePropertyS k e

isEdgePropertyEqual :: Key -> Value -> Edge -> GS Bool
isEdgePropertyEqual k v e = return $ isEdgePropertyEqualS k v e

hasEdgeLabelIndexS :: LabelIndex -> Edge -> Bool
hasEdgeLabelIndexS li e = edgeLabelIndex e == li

hasEdgeLabelIndex :: LabelIndex -> Edge -> GS Bool
hasEdgeLabelIndex li e = return $ hasEdgeLabelIndexS li e

getStartNode :: Edge -> GS Node
getStartNode e = getNodeByIdUnsafe $ fst $ connection e

getEndNode :: Edge -> GS Node
getEndNode e = getNodeByIdUnsafe $ fst $ connection e
