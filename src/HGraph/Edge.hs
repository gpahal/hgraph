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


createEdgeNUnsafe :: Label -> Node -> Node -> GS (Edge, Node, Node)
createEdgeNUnsafe l sn en = do i <- incrementEdgeId
                               eli <- createEdgeLabel l
                               let e = emptyEdge eli i sn en
                               n1 <- addOutEdge e en eli sn
                               n2 <- addInEdge e sn eli en
                               se <- saveEdge e
                               return (se, n1, n2)

createEdge :: Label -> Id -> Id -> GS (Edge, Node, Node)
createEdge l snid enid = do sn <- getNodeByIdUnsafe snid
                            en <- getNodeByIdUnsafe enid
                            createEdgeNUnsafe l sn en

createEdgeN :: Label -> Node -> Node -> GS (Edge, Node, Node)
createEdgeN l sn en = createEdge l (nodeId sn) (nodeId en)

createEdgePair :: Label -> Id -> Id -> GS (Edge, Edge, Node, Node)
createEdgePair l snid enid = do (e1, _, _) <- createEdge l snid enid
                                (e2, en2, sn2) <- createEdge l enid snid
                                return (e1, e2, sn2, en2)

createEdgeNPair :: Label -> Node -> Node -> GS (Edge, Edge, Node, Node)
createEdgeNPair l sn en = do (e1, sn1, en1) <- createEdgeN l sn en
                             (e2, en2, sn2) <- createEdgeN l en1 sn1
                             return (e1, e2, sn2, en2)

createEdgeNPairUnsafe :: Label -> Node -> Node -> GS (Edge, Edge, Node, Node)
createEdgeNPairUnsafe l sn en = do (e1, sn1, en1) <- createEdgeNUnsafe l sn en
                                   (e2, en2, sn2) <- createEdgeNUnsafe l en1 sn1
                                   return (e1, e2, sn2, en2)


setEdgePropertyE :: Key -> Value -> Edge -> GS Edge
setEdgePropertyE k v e = if k `elem` edgeKeyBlacklist then return e else saveEdge newEdge
    where
        newEdge  = alterEdgeProperties (M.insert k v $ edgeProperties e) e

setEdgeProperty :: Key -> Value -> Id -> GS Edge
setEdgeProperty k v i = getEdgeByIdUnsafe i >>= setEdgePropertyE k v

removeEdgePropertyE :: Key -> Edge -> GS Edge
removeEdgePropertyE k e = if k `elem` nodeKeyBlacklist then return e else saveEdge newEdge
    where
        newEdge  = alterEdgeProperties (M.delete k $ edgeProperties e) e

removeEdgeProperty :: Key -> Id -> GS Edge
removeEdgeProperty k i = getEdgeByIdUnsafe i >>= removeEdgePropertyE k

getEdgePropertySE :: Key -> Edge -> Maybe Value
getEdgePropertySE k e = M.lookup k (edgeProperties e)

getEdgePropertyE :: Key -> Edge -> GS (Maybe Value)
getEdgePropertyE k e = return $ getEdgePropertySE k e

getEdgeProperty :: Key -> Id -> GS (Maybe Value)
getEdgeProperty k i = getEdgeByIdUnsafe i >>= getEdgePropertyE k

isEdgePropertyEqualSE :: Key -> Value -> Edge -> Bool
isEdgePropertyEqualSE k v e = maybe False (==v) $ getEdgePropertySE k e

isEdgePropertyEqualE :: Key -> Value -> Edge -> GS Bool
isEdgePropertyEqualE k v e = return $ isEdgePropertyEqualSE k v e

isEdgePropertyEqual :: Key -> Value -> Id -> GS Bool
isEdgePropertyEqual k v i = getEdgeByIdUnsafe i >>= isEdgePropertyEqualE k v

hasEdgeLabelIndexSE :: LabelIndex -> Edge -> Bool
hasEdgeLabelIndexSE li e = edgeLabelIndex e == li

hasEdgeLabelIndexE :: LabelIndex -> Edge -> GS Bool
hasEdgeLabelIndexE li e = return $ hasEdgeLabelIndexSE li e

hasEdgeLabelIndex :: LabelIndex -> Id -> GS Bool
hasEdgeLabelIndex li i = getEdgeByIdUnsafe i >>= hasEdgeLabelIndexE li

edgeLabelE :: Edge -> GS Label
edgeLabelE e = do l <- getEdgeLabel $ edgeLabelIndex e
                  return $ MB.fromMaybe (error "incorrect edge in function edgeLabel") l

edgeLabel :: Id -> GS Label
edgeLabel i = getEdgeByIdUnsafe i >>= edgeLabelE

edgeLabelSE :: Graph -> Edge -> Label
edgeLabelSE = unpackStateValue edgeLabelE

edgeLabelS :: Graph -> Id -> Label
edgeLabelS = unpackStateValue edgeLabel

changeEdgeLabelE :: Label -> Edge -> GS Edge
changeEdgeLabelE l e = do let oeli = edgeLabelIndex e
                          neli <- createEdgeLabel l
                          ne <- saveEdge $ alterEdgeLabelIndex neli e
                          sn <- getStartNodeN e
                          en <- getEndNodeE e
                          sn1 <- removeOutEdge e oeli sn
                          en1 <- removeOutEdge e oeli en
                          _ <- addOutEdge ne en1 neli sn1
                          _ <- addInEdge ne sn1 neli en1
                          return ne

changeEdgeLabel :: Label -> Id -> GS Edge
changeEdgeLabel l i = getEdgeByIdUnsafe i >>= changeEdgeLabelE l


getStartNodeN :: Edge -> GS Node
getStartNodeN e = getNodeByIdUnsafe $ fst $ connection e

getStartNode :: Id -> GS Node
getStartNode i = getEdgeByIdUnsafe i >>= getStartNodeN

getEndNodeE :: Edge -> GS Node
getEndNodeE e = getNodeByIdUnsafe $ fst $ connection e

getEndNode :: Id -> GS Node
getEndNode i = getEdgeByIdUnsafe i >>= getEndNodeE

deleteEdge :: Edge -> GS ()
deleteEdge e = do g <- get
                  let eid = edgeId e
                  let eli = edgeLabelIndex e
                  alterEdges $ M.delete eid (edges g)
                  sn <- getStartNodeN e
                  en <- getEndNodeE e
                  _ <- removeOutEdge e eli sn
                  _ <- removeInEdge e eli en
                  return ()

deleteNodeN :: Node -> GS ()
deleteNodeN n = do es <- getAllEdges n
                   foldl (>>) (return ()) $ map deleteEdge es
                   deleteNode' n

deleteNode :: Id -> GS ()
deleteNode i = getNodeByIdUnsafe i >>= deleteNodeN
