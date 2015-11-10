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


hasNodeLabelIndexS :: LabelIndex -> Node -> Bool
hasNodeLabelIndexS li n = S.member li $ nodeLabelIndices n

hasNodeLabelIndex :: LabelIndex -> Node -> GS Bool
hasNodeLabelIndex li n = return $ hasNodeLabelIndexS li n

hasNodeLabel :: Label -> Node -> GS Bool
hasNodeLabel l n = do li <- getNodeLabelIndex l
                      return $ maybe False (`hasNodeLabelIndexS` n) li

nodeLabels :: Node -> GS [Label]
nodeLabels n = do g <- get
                  let lis = S.elems $ nodeLabelIndices n
                  return $ map (MB.fromMaybe (error "incorrect edge in function nodeLabels") . unpackStateValue getNodeLabel g) lis

nodeLabelsS :: Graph -> Node -> [Label]
nodeLabelsS = unpackStateValue nodeLabels

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


removeLabelIndexFromNode :: LabelIndex -> Node -> GS Node
removeLabelIndexFromNode li n = do g <- get
                                   alterRemoveLabelInstance (nodeLabelInstances g) li n
                                   saveNode $ alterNodeLabelIndices (S.delete li $ nodeLabelIndices n) n

removeLabelIndicesFromNode :: S.Set LabelIndex -> Node -> GS Node
removeLabelIndicesFromNode lis n = do g <- get
                                      alterRemoveLabelInstances (nodeLabelInstances g) lis n
                                      saveNode $ alterNodeLabelIndices (S.difference (nodeLabelIndices n) lis) n

removeAllLabelIndicesFromNode :: Node -> GS Node
removeAllLabelIndicesFromNode n = removeLabelIndicesFromNode (nodeLabelIndices n) n

removeLabelFromNode :: Label -> Node -> GS Node
removeLabelFromNode l n = do g <- get
                             nli <- getNodeLabelIndex l
                             maybe (return n) (`removeLabelIndexFromNode` n) nli

removeLabelsFromNode :: S.Set Label -> Node -> GS Node
removeLabelsFromNode ls n = do g <- get
                               let lis = S.foldl (\a b -> maybe a (`S.insert` a) b) S.empty $ S.map (getNodeLabelIndexS g) ls
                               removeLabelIndicesFromNode lis n

createNode :: GS Node
createNode = do i <- incrementNodeId
                saveNode $ emptyNode i

createNodeWithLabel :: Label -> GS Node
createNodeWithLabel l = createNode >>= addLabelToNode l

createNodeWithLabels :: S.Set Label -> GS Node
createNodeWithLabels ls = createNode >>= addLabelsToNode ls


setNodeProperty :: Key -> Value -> Node -> GS Node
setNodeProperty k v n = if k `elem` nodeKeyBlacklist then return n else saveNode newNode
    where
        newNode  = alterNodeProperties (M.insert k v $ nodeProperties n) n

removeNodeProperty :: Key -> Node -> GS Node
removeNodeProperty k n = if k `elem` nodeKeyBlacklist then return n else saveNode newNode
    where
        newNode  = alterNodeProperties (M.delete k $ nodeProperties n) n

getNodePropertyS :: Key -> Node -> Maybe Value
getNodePropertyS k n = M.lookup k (nodeProperties n)

getNodeProperty :: Key -> Node -> GS (Maybe Value)
getNodeProperty k n = return $ getNodePropertyS k n

isNodePropertyEqualS :: Key -> Value -> Node -> Bool
isNodePropertyEqualS k v n = maybe False (==v) $ getNodePropertyS k n

isNodePropertyEqual :: Key -> Value -> Node -> GS Bool
isNodePropertyEqual k v n = return $ isNodePropertyEqualS k v n

getLabelIndexOutEdges :: LabelIndex -> Node -> GS [Edge]
getLabelIndexOutEdges li n = do g <- get
                                let es = edges g
                                return $ maybe [] (map (\x -> MB.fromJust $ M.lookup x es) . M.keys) $ M.lookup li $ outEdges n

getLabelIndexInEdges :: LabelIndex -> Node -> GS [Edge]
getLabelIndexInEdges li n = do g <- get
                               let es = edges g
                               return $ maybe [] (map (\x -> MB.fromJust $ M.lookup x es) . M.keys) $ M.lookup li $ inEdges n

getLabelOutEdges :: Label -> Node -> GS [Edge]
getLabelOutEdges l n = do li <- getNodeLabelIndex l
                          maybe (return []) (`getLabelIndexOutEdges` n) li

getLabelInEdges :: Label -> Node -> GS [Edge]
getLabelInEdges l n = do li <- getNodeLabelIndex l
                         maybe (return []) (`getLabelIndexInEdges` n) li

getAllOutEdges :: Node -> GS [Edge]
getAllOutEdges n = do g <- get
                      let es = edges g
                      return $ map (\x -> MB.fromJust $ M.lookup x es) $ M.foldl (\a b -> a ++ M.keys b) [] $ outEdges n

getAllInEdges :: Node -> GS [Edge]
getAllInEdges n = do g <- get
                     let es = edges g
                     return $ map (\x -> MB.fromJust $ M.lookup x es) $ M.foldl (\a b -> a ++ M.keys b) [] $ inEdges n

getAllEdges :: Node -> GS [Edge]
getAllEdges n = do oes <- getAllOutEdges n
                   ies <- getAllInEdges n
                   return $ oes ++ ies

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

getOutEdges :: (Label -> Bool) -> Node -> GS [Edge]
getOutEdges f n = do g <- get
                     let oes = filter (unpackStateValue (getNodeLabelIndexFilter f) g) $ M.keys $ outEdges n
                     getOutEdges' oes n

getInEdges :: (Label -> Bool) -> Node -> GS [Edge]
getInEdges f n = do g <- get
                    let ies = filter (unpackStateValue (getNodeLabelIndexFilter f) g) $ M.keys $ inEdges n
                    getInEdges' ies n

getEdges :: (Label -> Bool) -> Node -> GS [Edge]
getEdges f n = do oes <- getOutEdges f n
                  ies <- getInEdges f n
                  return $ oes ++ ies

getFilteredOutEdges :: (Edge -> Bool) -> (Label -> Bool) -> Node -> GS [Edge]
getFilteredOutEdges ef lf n = filter ef <$> getOutEdges lf n

getFilteredInEdges :: (Edge -> Bool) -> (Label -> Bool) -> Node -> GS [Edge]
getFilteredInEdges ef lf n = filter ef <$> getInEdges lf n

getFilteredEdges :: (Edge -> Bool) -> (Label -> Bool) -> Node -> GS [Edge]
getFilteredEdges ef lf n = do oes <- getFilteredOutEdges ef lf n
                              ies <- getFilteredInEdges ef lf n
                              return $ oes ++ ies

deleteNode' :: Node -> GS ()
deleteNode' n = do g <- get
                   _ <- removeAllLabelIndicesFromNode n
                   let ns = nodes g
                   alterNodes $ M.delete (nodeId n) ns
