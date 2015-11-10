module HGraph.Edge where

import           Control.Monad.State
import qualified Data.Map            as M
import qualified Data.Maybe          as MB
import qualified Data.Text           as T
import           HGraph.Graph
import           HGraph.Label
import           HGraph.Node
import           HGraph.Types

edgeKeyBlacklist :: [T.Text]
edgeKeyBlacklist = map T.pack [idKey]

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

createEdgePair :: Label -> Node -> Node -> GS (Edge, Edge)
createEdgePair l sn en = do e1 <- createEdge l sn en
                            e2 <- createEdge l sn en
                            return (e1, e2)

setEdgeProperty :: Key -> Value -> Edge -> GS Edge
setEdgeProperty k v e = if k `elem` edgeKeyBlacklist then return e else saveEdge newEdge
    where
        newEdge  = alterEdgeProperties (M.insert k v $ edgeProperties e) e

removeEdgeProperty :: Key -> Edge -> GS Edge
removeEdgeProperty k e = if k `elem` nodeKeyBlacklist then return e else saveEdge newEdge
    where
        newEdge  = alterEdgeProperties (M.delete k $ edgeProperties e) e

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

edgeLabel :: Edge -> GS Label
edgeLabel e = do l <- getNodeLabel $ edgeLabelIndex e
                 return $ MB.fromMaybe (error "incorrect edge in function edgeLabel") l

edgeLabelS :: Graph -> Edge -> Label
edgeLabelS = unpackStateValue edgeLabel

getStartNode :: Edge -> GS Node
getStartNode e = getNodeByIdUnsafe $ fst $ connection e

getEndNode :: Edge -> GS Node
getEndNode e = getNodeByIdUnsafe $ fst $ connection e

deleteEdge :: Edge -> GS ()
deleteEdge e = do g <- get
                  let eid = edgeId e
                  let eli = edgeLabelIndex e
                  alterEdges $ M.delete eid (edges g)
                  sn <- getStartNode e
                  en <- getEndNode e
                  _ <- saveNode $ removeOutEdge e eli sn
                  _ <- saveNode $ removeInEdge e eli en
                  return ()

deleteNode :: Node -> GS ()
deleteNode n = do es <- getAllEdges n
                  foldl (>>) (return ()) $ map deleteEdge es
                  deleteNode' n

changeEdgeLabel :: Label -> Edge -> GS Edge
changeEdgeLabel l e = do let oeli = edgeLabelIndex e
                         neli <- createEdgeLabel l
                         ne <- saveEdge $ alterEdgeLabelIndex neli e
                         sn <- getStartNode e
                         en <- getEndNode e
                         _ <- saveNode $ addOutEdge ne en neli $ removeOutEdge e oeli sn
                         _ <- saveNode $ addInEdge ne sn neli $ removeInEdge e oeli en
                         return ne
