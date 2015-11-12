module HGraph.Node where

import           Control.Applicative
import           Control.Monad.State
import qualified Data.Map            as M
import qualified Data.Maybe          as MB
import qualified Data.Set            as S
import qualified Data.Text           as T
import           HGraph.Graph
import           HGraph.Label
import           HGraph.Types

nodeKeyBlacklist :: [Key]
nodeKeyBlacklist = map T.pack [idKey]

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


addOutEdge :: Edge -> Node -> LabelIndex -> Node -> GS Node
addOutEdge e en li n = saveNode $ alterOutEdges nbs n
    where
        oe  = outEdges n
        s   = MB.fromMaybe M.empty $ M.lookup li oe
        nbs = M.insert li (M.insert (edgeId e) (nodeId en) s) oe

addInEdge :: Edge -> Node -> LabelIndex -> Node -> GS Node
addInEdge e sn li n = saveNode $ alterInEdges nbs n
    where
        ie  = inEdges n
        s   = MB.fromMaybe M.empty $ M.lookup li ie
        nbs = M.insert li (M.insert (edgeId e) (nodeId sn) s) ie


removeOutEdge :: Edge -> LabelIndex -> Node -> GS Node
removeOutEdge e li n = saveNode $ alterOutEdges nbs n
    where
        oe  = outEdges n
        s   = MB.fromMaybe M.empty $ M.lookup li oe
        nbs = M.insert li (M.delete (edgeId e) s) oe

removeInEdge :: Edge -> LabelIndex -> Node -> GS Node
removeInEdge e li n = saveNode $ alterInEdges nbs n
    where
        ie  = inEdges n
        s   = MB.fromMaybe M.empty $ M.lookup li ie
        nbs = M.insert li (M.delete (edgeId e) s) ie


hasNodeLabelIndexSN :: LabelIndex -> Node -> Bool
hasNodeLabelIndexSN li n = S.member li $ nodeLabelIndices n

hasNodeLabelIndexN :: LabelIndex -> Node -> GS Bool
hasNodeLabelIndexN li n = return $ hasNodeLabelIndexSN li n

hasNodeLabelIndex :: LabelIndex -> Id -> GS Bool
hasNodeLabelIndex li i = getNodeByIdUnsafe i >>= hasNodeLabelIndexN li

hasNodeLabelN :: Label -> Node -> GS Bool
hasNodeLabelN l n = do li <- getNodeLabelIndex l
                       return $ maybe False (`hasNodeLabelIndexSN` n) li

hasNodeLabel :: Label -> Id -> GS Bool
hasNodeLabel l i = getNodeByIdUnsafe i >>= hasNodeLabelN l

nodeLabelsN :: Node -> GS [Label]
nodeLabelsN n = do g <- get
                   let lis = S.elems $ nodeLabelIndices n
                   return $ map (MB.fromMaybe (error "incorrect edge in function nodeLabels") . unpackStateValue getNodeLabel g) lis

nodeLabels :: Id -> GS [Label]
nodeLabels i = getNodeByIdUnsafe i >>= nodeLabelsN

nodeLabelsSN :: Graph -> Node -> [Label]
nodeLabelsSN = unpackStateValue nodeLabelsN

nodeLabelsS :: Graph -> Id -> [Label]
nodeLabelsS = unpackStateValue nodeLabels

addLabelToNodeN :: Label -> Node -> GS Node
addLabelToNodeN l n = do g <- get
                         nli <- createNodeLabel l
                         alterAddLabelInstance (nodeLabelInstances g) nli n
                         saveNode $ alterNodeLabelIndices (S.insert nli $ nodeLabelIndices n) n

addLabelToNode :: Label -> Id -> GS Node
addLabelToNode l i = getNodeByIdUnsafe i >>= addLabelToNodeN l

addLabelsToNodeN :: S.Set Label -> Node -> GS Node
addLabelsToNodeN ls n = do g <- get
                           nlis <- createNodeLabels ls
                           alterAddLabelInstances (nodeLabelInstances g) nlis n
                           saveNode $ alterNodeLabelIndices (S.union nlis $ nodeLabelIndices n) n

addLabelsToNode :: S.Set Label -> Id -> GS Node
addLabelsToNode ls i = getNodeByIdUnsafe i >>= addLabelsToNodeN ls


removeLabelIndexFromNodeN :: LabelIndex -> Node -> GS Node
removeLabelIndexFromNodeN li n = do g <- get
                                    alterRemoveLabelInstance (nodeLabelInstances g) li n
                                    saveNode $ alterNodeLabelIndices (S.delete li $ nodeLabelIndices n) n

removeLabelIndexFromNode :: LabelIndex -> Id -> GS Node
removeLabelIndexFromNode li i = getNodeByIdUnsafe i >>= removeLabelIndexFromNodeN li

removeLabelIndicesFromNodeN :: S.Set LabelIndex -> Node -> GS Node
removeLabelIndicesFromNodeN lis n = do g <- get
                                       alterRemoveLabelInstances (nodeLabelInstances g) lis n
                                       saveNode $ alterNodeLabelIndices (S.difference (nodeLabelIndices n) lis) n

removeLabelIndicesFromNode :: S.Set LabelIndex -> Id -> GS Node
removeLabelIndicesFromNode lis i = getNodeByIdUnsafe i >>= removeLabelIndicesFromNodeN lis

removeAllLabelIndicesFromNodeN :: Node -> GS Node
removeAllLabelIndicesFromNodeN n = removeLabelIndicesFromNodeN (nodeLabelIndices n) n

removeAllLabelIndicesFromNode :: Id -> GS Node
removeAllLabelIndicesFromNode i = getNodeByIdUnsafe i >>= removeAllLabelIndicesFromNodeN

removeLabelFromNodeN :: Label -> Node -> GS Node
removeLabelFromNodeN l n = do nli <- getNodeLabelIndex l
                              maybe (return n) (`removeLabelIndexFromNodeN` n) nli

removeLabelFromNode :: Label -> Id -> GS Node
removeLabelFromNode l i = getNodeByIdUnsafe i >>= removeLabelFromNodeN l

removeLabelsFromNodeN :: S.Set Label -> Node -> GS Node
removeLabelsFromNodeN ls n = do g <- get
                                let lis = S.foldl (\a b -> maybe a (`S.insert` a) b) S.empty $ S.map (getNodeLabelIndexS g) ls
                                removeLabelIndicesFromNodeN lis n

removeLabelsFromNode :: S.Set Label -> Id -> GS Node
removeLabelsFromNode ls i = getNodeByIdUnsafe i >>= removeLabelsFromNodeN ls


createNodeRN :: GS Node
createNodeRN = do i <- incrementNodeId
                  saveNode $ emptyNode i

createNode :: GS Id
createNode = nodeId <$> createNodeRN

createNodeWithLabelRN :: Label -> GS Node
createNodeWithLabelRN l = createNodeRN >>= addLabelToNodeN l

createNodeWithLabel :: Label -> GS Id
createNodeWithLabel l = nodeId <$> createNodeWithLabelRN l

createNodeWithLabelsRN :: S.Set Label -> GS Node
createNodeWithLabelsRN ls = createNodeRN >>= addLabelsToNodeN ls

createNodeWithLabels :: S.Set Label -> GS Id
createNodeWithLabels ls = nodeId <$> createNodeWithLabelsRN ls


setNodePropertyN :: Key -> Value -> Node -> GS Node
setNodePropertyN k v n = if k `elem` nodeKeyBlacklist then return n else saveNode newNode
    where
        newNode  = alterNodeProperties (M.insert k v $ nodeProperties n) n

setNodeProperty :: Key -> Value -> Id -> GS Node
setNodeProperty k v i = getNodeByIdUnsafe i >>= setNodePropertyN k v

setNodePropertiesN :: [(Key, Value)] -> Node -> GS Node
setNodePropertiesN kvs n = foldl (\a (k, v) -> a >>= setNodePropertyN k v) (return n) kvs

setNodeProperties :: [(Key, Value)] -> Id -> GS Node
setNodeProperties kvs i = getNodeByIdUnsafe i >>= setNodePropertiesN kvs

removeNodePropertyN :: Key -> Node -> GS Node
removeNodePropertyN k n = if k `elem` nodeKeyBlacklist then return n else saveNode newNode
    where
        newNode  = alterNodeProperties (M.delete k $ nodeProperties n) n

removeNodeProperty :: Key -> Id -> GS Node
removeNodeProperty k i = getNodeByIdUnsafe i >>= removeNodePropertyN k

getNodePropertySN :: Key -> Node -> Maybe Value
getNodePropertySN k n = M.lookup k (nodeProperties n)

getNodePropertyN :: Key -> Node -> GS (Maybe Value)
getNodePropertyN k n = return $ getNodePropertySN k n

getNodeProperty :: Key -> Id -> GS (Maybe Value)
getNodeProperty k i = getNodeByIdUnsafe i >>= getNodePropertyN k

isNodePropertyEqualSN :: Key -> Value -> Node -> Bool
isNodePropertyEqualSN k v n = maybe False (==v) $ getNodePropertySN k n

isNodePropertyEqualN :: Key -> Value -> Node -> GS Bool
isNodePropertyEqualN k v n = return $ isNodePropertyEqualSN k v n

isNodePropertyEqual :: Key -> Value -> Id -> GS Bool
isNodePropertyEqual k v i = getNodeByIdUnsafe i >>= isNodePropertyEqualN k v


getLabelIndexOutEdgesN :: LabelIndex -> Node -> GS [Edge]
getLabelIndexOutEdgesN li n = do g <- get
                                 let es = edges g
                                 return $ maybe [] (map (\x -> MB.fromJust $ M.lookup x es) . M.keys) $ M.lookup li $ outEdges n

getLabelIndexOutEdges :: LabelIndex -> Id -> GS [Edge]
getLabelIndexOutEdges li i = getNodeByIdUnsafe i >>= getLabelIndexOutEdgesN li

getLabelIndexInEdgesN :: LabelIndex -> Node -> GS [Edge]
getLabelIndexInEdgesN li n = do g <- get
                                let es = edges g
                                return $ maybe [] (map (\x -> MB.fromJust $ M.lookup x es) . M.keys) $ M.lookup li $ inEdges n

getLabelIndexInEdges :: LabelIndex -> Id -> GS [Edge]
getLabelIndexInEdges li i = getNodeByIdUnsafe i >>= getLabelIndexInEdgesN li

getLabelOutEdgesN :: Label -> Node -> GS [Edge]
getLabelOutEdgesN l n = do li <- getNodeLabelIndex l
                           maybe (return []) (`getLabelIndexOutEdgesN` n) li

getLabelOutEdges :: Label -> Id -> GS [Edge]
getLabelOutEdges l i = getNodeByIdUnsafe i >>= getLabelOutEdgesN l

getLabelInEdgesN :: Label -> Node -> GS [Edge]
getLabelInEdgesN l n = do li <- getNodeLabelIndex l
                          maybe (return []) (`getLabelIndexInEdgesN` n) li

getLabelInEdges :: Label -> Id -> GS [Edge]
getLabelInEdges l i = getNodeByIdUnsafe i >>= getLabelInEdgesN l

getAllOutEdgesN :: Node -> GS [Edge]
getAllOutEdgesN n = do g <- get
                       let es = edges g
                       return $ map (\x -> MB.fromJust $ M.lookup x es) $ M.foldl (\a b -> a ++ M.keys b) [] $ outEdges n

getAllOutEdges :: Id -> GS [Edge]
getAllOutEdges i = getNodeByIdUnsafe i >>= getAllOutEdgesN

getAllInEdgesN :: Node -> GS [Edge]
getAllInEdgesN n = do g <- get
                      let es = edges g
                      return $ map (\x -> MB.fromJust $ M.lookup x es) $ M.foldl (\a b -> a ++ M.keys b) [] $ inEdges n

getAllInEdges :: Id -> GS [Edge]
getAllInEdges i = getNodeByIdUnsafe i >>= getAllInEdgesN

getAllEdgesN :: Node -> GS [Edge]
getAllEdgesN n = do oes <- getAllOutEdgesN n
                    ies <- getAllInEdgesN n
                    return $ oes ++ ies

getAllEdges :: Id -> GS [Edge]
getAllEdges i = getNodeByIdUnsafe i >>= getAllEdgesN

getOutEdges' :: [LabelIndex] -> Node -> GS [Edge]
getOutEdges' lis n = do g <- get
                        let es = edges g
                        let oes = outEdges n
                        return $ foldl (\a li -> a ++ maybe [] (map (\x -> MB.fromJust $ M.lookup x es) . M.keys) (M.lookup li oes)) [] lis

getInEdges' :: [LabelIndex] -> Node -> GS [Edge]
getInEdges' lis n = do g <- get
                       let es = edges g
                       let ies = inEdges n
                       return $ foldl (\a li -> a ++ maybe [] (map (\x -> MB.fromJust $ M.lookup x es) . M.keys) (M.lookup li ies)) [] lis

getOutEdgesN :: (Label -> Bool) -> Node -> GS [Edge]
getOutEdgesN f n = do g <- get
                      let oes = filter (unpackStateValue (getNodeLabelIndexFilter f) g) $ M.keys $ outEdges n
                      getOutEdges' oes n

getOutEdges :: (Label -> Bool) -> Id -> GS [Edge]
getOutEdges f i = getNodeByIdUnsafe i >>= getOutEdgesN f

getInEdgesN :: (Label -> Bool) -> Node -> GS [Edge]
getInEdgesN f n = do g <- get
                     let ies = filter (unpackStateValue (getNodeLabelIndexFilter f) g) $ M.keys $ inEdges n
                     getInEdges' ies n

getInEdges :: (Label -> Bool) -> Id -> GS [Edge]
getInEdges f i = getNodeByIdUnsafe i >>= getInEdgesN f

getEdgesN :: (Label -> Bool) -> Node -> GS [Edge]
getEdgesN f n = do oes <- getOutEdgesN f n
                   ies <- getInEdgesN f n
                   return $ oes ++ ies

getEdges :: (Label -> Bool) -> Id -> GS [Edge]
getEdges f i = getNodeByIdUnsafe i >>= getEdgesN f

getFilteredOutEdgesN :: (Edge -> Bool) -> (Label -> Bool) -> Node -> GS [Edge]
getFilteredOutEdgesN ef lf n = filter ef <$> getOutEdgesN lf n

getFilteredOutEdges :: (Edge -> Bool) -> (Label -> Bool) -> Id -> GS [Edge]
getFilteredOutEdges ef lf i = getNodeByIdUnsafe i >>= getFilteredOutEdgesN ef lf

getFilteredInEdgesN :: (Edge -> Bool) -> (Label -> Bool) -> Node -> GS [Edge]
getFilteredInEdgesN ef lf n = filter ef <$> getInEdgesN lf n

getFilteredInEdges :: (Edge -> Bool) -> (Label -> Bool) -> Id -> GS [Edge]
getFilteredInEdges ef lf i = getNodeByIdUnsafe i >>= getFilteredInEdgesN ef lf

getFilteredEdgesN :: (Edge -> Bool) -> (Label -> Bool) -> Node -> GS [Edge]
getFilteredEdgesN ef lf n = do oes <- getFilteredOutEdgesN ef lf n
                               ies <- getFilteredInEdgesN ef lf n
                               return $ oes ++ ies

getFilteredEdges :: (Edge -> Bool) -> (Label -> Bool) -> Id -> GS [Edge]
getFilteredEdges ef lf i = getNodeByIdUnsafe i >>= getFilteredEdgesN ef lf


deleteNode' :: Node -> GS ()
deleteNode' n = do g <- get
                   _ <- removeAllLabelIndicesFromNodeN n
                   let ns = nodes g
                   alterNodes $ M.delete (nodeId n) ns
