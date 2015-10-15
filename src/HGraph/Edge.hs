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


alterEdgeLabelIndex :: Edge -> LabelIndex -> Edge
alterEdgeLabelIndex e li = Edge li (edgeId e) (edgeProperties e) (connection e)

alterEdgeProperties :: Edge -> Properties -> Edge
alterEdgeProperties e p = Edge (edgeLabelIndex e) (edgeId e) p (connection e)


createEdge :: Node -> Node -> Label -> GS Edge
createEdge sn en l = do i <- incrementEdgeId
                        eli <- createEdgeLabel l
                        let e = emptyEdge eli i sn en
                        _ <- saveNode $ addOutEdge sn e en eli
                        _ <- saveNode $ addInEdge en e sn eli
                        saveEdge e


setEdgeProperty :: Key -> Value -> Edge -> GS Edge
setEdgeProperty k v e = saveEdge newEdge
    where
        newEdge  = alterEdgeProperties e $ M.insert k v $ edgeProperties e

getEdgeProperty :: Key -> Edge -> Maybe Value
getEdgeProperty k e = M.lookup k (edgeProperties e)
