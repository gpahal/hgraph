module HGraph.Query where

import           Control.Applicative
import           Control.Monad.State
import qualified Data.Map            as M
import           Data.Maybe
import qualified Data.Set            as S
import           HGraph.Edge
import           HGraph.Graph
import           HGraph.Label
import           HGraph.Node
import           HGraph.Types

findNodes :: (Node -> Bool) -> GS [Node]
findNodes f = do g <- get
                 return $ filter f $ M.elems $ nodes g

findLabelNodes :: Label -> (Node -> Bool) -> GS [Node]
findLabelNodes l f = do g <- get
                        li <- getNodeLabelIndex l
                        let nlins = maybe Nothing (\val -> M.lookup val $ nodeLabelInstances g) li
                        let ns = nodes g
                        let aux nid = fromJust $ M.lookup nid ns
                        return $ maybe [] (filter f . map aux . S.elems) nlins

findNode :: (Node -> Bool) -> GS (Maybe Node)
findNode f = aux <$> findNodes f
    where
        aux ns = if null ns then Nothing else Just $ head ns

findLabelNode :: Label -> (Node -> Bool) -> GS (Maybe Node)
findLabelNode l f = aux <$> findLabelNodes l f
    where
        aux ns = if null ns then Nothing else Just $ head ns

findNodeById :: Id -> GS (Maybe Node)
findNodeById = getNodeById

findLabelNodeById :: Label -> Id -> GS (Maybe Node)
findLabelNodeById l i = do g <- findNodeById i
                           li <- getNodeLabelIndex l
                           return $ maybe Nothing (\n -> maybe Nothing (\val -> if hasNodeLabelIndexSN val n then Just n else Nothing) li) g

findEdgeById :: Id -> GS (Maybe Edge)
findEdgeById = getEdgeById

findLabelEdgeById :: Label -> Id -> GS (Maybe Edge)
findLabelEdgeById l i = do g <- findEdgeById i
                           li <- getEdgeLabelIndex l
                           return $ maybe Nothing (\n -> maybe Nothing (\val -> if hasEdgeLabelIndexSE val n then Just n else Nothing) li) g

findNodesByProperty :: Key -> Value -> GS [Node]
findNodesByProperty k v = findNodes $ isNodePropertyEqualSN k v

findLabelNodesByProperty :: Label -> Key -> Value -> GS [Node]
findLabelNodesByProperty l k v = findLabelNodes l $ isNodePropertyEqualSN k v

findNodeByProperty :: Key -> Value -> GS (Maybe Node)
findNodeByProperty k v = findNode $ isNodePropertyEqualSN k v

findLabelNodeByProperty :: Label -> Key -> Value -> GS (Maybe Node)
findLabelNodeByProperty l k v = findLabelNode l $ isNodePropertyEqualSN k v

getNodesByEdgesE :: (Edge -> GS Node) -> GS [Edge] -> GS [(Edge, Node)]
getNodesByEdgesE f gs = do g <- get
                           es <- gs
                           return $ map (\v -> (v, unpackStateValue f g v)) es

getNodesByEdges :: (Edge -> GS Node) -> GS [Id] -> GS [(Edge, Node)]
getNodesByEdges f gs = do g <- get
                          is <- gs
                          let es = map (unpackStateValue getEdgeByIdUnsafe g) is
                          return $ map (\v -> (v, unpackStateValue f g v)) es

getLabelOutNodesN :: Label -> Node -> GS [(Edge, Node)]
getLabelOutNodesN l = getNodesByEdgesE getEndNodeE . getLabelOutEdgesN l

getLabelOutNodes :: Label -> Id -> GS [(Edge, Node)]
getLabelOutNodes l i = getNodeByIdUnsafe i >>= getLabelOutNodesN l

getLabelInNodesN :: Label -> Node -> GS [(Edge, Node)]
getLabelInNodesN l = getNodesByEdgesE getStartNodeN . getLabelInEdgesN l

getLabelInNodes :: Label -> Id -> GS [(Edge, Node)]
getLabelInNodes l i = getNodeByIdUnsafe i >>= getLabelInNodesN l

getAllOutNodesN :: Node -> GS [(Edge, Node)]
getAllOutNodesN = getNodesByEdgesE getEndNodeE . getAllOutEdgesN

getAllOutNodes :: Id -> GS [(Edge, Node)]
getAllOutNodes i = getNodeByIdUnsafe i >>= getAllOutNodesN

getAllInNodesN :: Node -> GS [(Edge, Node)]
getAllInNodesN = getNodesByEdgesE getStartNodeN . getAllInEdgesN

getAllInNodes :: Id -> GS [(Edge, Node)]
getAllInNodes i = getNodeByIdUnsafe i >>= getAllInNodesN

getAllNodesN :: Node -> GS [(Edge, Node)]
getAllNodesN = getNodesByEdgesE getStartNodeN . getAllEdgesN

getAllNodes :: Id -> GS [(Edge, Node)]
getAllNodes i = getNodeByIdUnsafe i >>= getAllNodesN

getOutNodesN :: (Label -> Bool) -> Node -> GS [(Edge, Node)]
getOutNodesN f = getNodesByEdgesE getEndNodeE . getOutEdgesN f

getOutNodes :: (Label -> Bool) -> Id -> GS [(Edge, Node)]
getOutNodes f i = getNodeByIdUnsafe i >>= getOutNodesN f

getInNodesN :: (Label -> Bool) -> Node -> GS [(Edge, Node)]
getInNodesN f = getNodesByEdgesE getStartNodeN . getInEdgesN f

getInNodes :: (Label -> Bool) -> Id -> GS [(Edge, Node)]
getInNodes f i = getNodeByIdUnsafe i >>= getInNodesN f

getNodesN :: (Label -> Bool) -> Node -> GS [(Edge, Node)]
getNodesN f = getNodesByEdgesE getStartNodeN . getEdgesN f

getNodes :: (Label -> Bool) -> Id -> GS [(Edge, Node)]
getNodes f i = getNodeByIdUnsafe i >>= getNodesN f

getFilteredOutNodesN :: (Edge -> Bool) -> (Label -> Bool) -> Node -> GS [(Edge, Node)]
getFilteredOutNodesN ef lf = getNodesByEdgesE getEndNodeE . getFilteredOutEdgesN ef lf

getFilteredOutNodes :: (Edge -> Bool) -> (Label -> Bool) -> Id -> GS [(Edge, Node)]
getFilteredOutNodes ef lf i = getNodeByIdUnsafe i >>= getFilteredOutNodesN ef lf

getFilteredInNodesN :: (Edge -> Bool) -> (Label -> Bool) -> Node -> GS [(Edge, Node)]
getFilteredInNodesN ef lf = getNodesByEdgesE getStartNodeN . getFilteredInEdgesN ef lf

getFilteredInNodes :: (Edge -> Bool) -> (Label -> Bool) -> Id -> GS [(Edge, Node)]
getFilteredInNodes ef lf i = getNodeByIdUnsafe i >>= getFilteredInNodesN ef lf

getFilteredNodesN :: (Edge -> Bool) -> (Label -> Bool) -> Node -> GS [(Edge, Node)]
getFilteredNodesN ef lf = getNodesByEdgesE getEndNodeE . getFilteredEdgesN ef lf

getFilteredNodes :: (Edge -> Bool) -> (Label -> Bool) -> Id -> GS [(Edge, Node)]
getFilteredNodes ef lf i = getNodeByIdUnsafe i >>= getFilteredNodesN ef lf

hasEdgeLabels :: [Label] -> Edge -> GS Bool
hasEdgeLabels ls e = do g <- get
                        return $ foldl (\a l -> a && evalState (hasEdgeLabelE l e) g) True ls

hasNodeLabels :: [Label] -> Node -> GS Bool
hasNodeLabels ls n = do g <- get
                        return $ foldl (\a l -> a && evalState (hasNodeLabelN l n) g) True ls
