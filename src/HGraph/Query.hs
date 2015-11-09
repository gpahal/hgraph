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
                           return $ maybe Nothing (\n -> maybe Nothing (\val -> if hasNodeLabelIndexS val n then Just n else Nothing) li) g

findEdgeById :: Id -> GS (Maybe Edge)
findEdgeById = getEdgeById

findLabelEdgeById :: Label -> Id -> GS (Maybe Edge)
findLabelEdgeById l i = do g <- findEdgeById i
                           li <- getEdgeLabelIndex l
                           return $ maybe Nothing (\n -> maybe Nothing (\val -> if hasEdgeLabelIndexS val n then Just n else Nothing) li) g

findNodesByProperty :: Key -> Value -> GS [Node]
findNodesByProperty k v = findNodes $ isNodePropertyEqualS k v

findLabelNodesByProperty :: Label -> Key -> Value -> GS [Node]
findLabelNodesByProperty l k v = findLabelNodes l $ isNodePropertyEqualS k v

findNodeByProperty :: Key -> Value -> GS (Maybe Node)
findNodeByProperty k v = findNode $ isNodePropertyEqualS k v

findLabelNodeByProperty :: Label -> Key -> Value -> GS (Maybe Node)
findLabelNodeByProperty l k v = findLabelNode l $ isNodePropertyEqualS k v

getNodesByEdges :: (Edge -> GS Node) -> GS [Edge] -> GS [(Edge, Node)]
getNodesByEdges f gs = do g <- get
                          es <- gs
                          return $ map (\v -> (v, unpackStateValue f g v)) es

getLabelOutNodes :: Label -> Node -> GS [(Edge, Node)]
getLabelOutNodes l = getNodesByEdges getEndNode . getLabelOutEdges l

getLabelInNodes :: Label -> Node -> GS [(Edge, Node)]
getLabelInNodes l = getNodesByEdges getStartNode . getLabelInEdges l

getAllOutNodes :: Node -> GS [(Edge, Node)]
getAllOutNodes = getNodesByEdges getEndNode . getAllOutEdges

getAllInNodes :: Node -> GS [(Edge, Node)]
getAllInNodes = getNodesByEdges getStartNode . getAllInEdges

getOutNodes :: (Label -> Bool) -> Node -> GS [(Edge, Node)]
getOutNodes f = getNodesByEdges getEndNode . getOutEdges f

getInNodes :: (Label -> Bool) -> Node -> GS [(Edge, Node)]
getInNodes f = getNodesByEdges getStartNode . getInEdges f

getFilteredOutNodes :: (Edge -> Bool) -> (Label -> Bool) -> Node -> GS [(Edge, Node)]
getFilteredOutNodes ef lf = getNodesByEdges getEndNode . getFilteredOutEdges ef lf

getFilteredInNodes :: (Edge -> Bool) -> (Label -> Bool) -> Node -> GS [(Edge, Node)]
getFilteredInNodes ef lf = getNodesByEdges getStartNode . getFilteredInEdges ef lf
