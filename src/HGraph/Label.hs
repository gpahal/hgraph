module HGraph.Label where

import           Control.Applicative
import           Control.Monad.Trans.State.Lazy
import qualified Data.Map                       as M
import qualified Data.Set                       as S
import           HGraph.Graph
import           HGraph.Types

getNodeLabelIndexMap :: GS LabelIndexMap
getNodeLabelIndexMap = fst . labelIndexMaps <$> get

getNodeLabelMap :: GS LabelMap
getNodeLabelMap = fst . labelMaps <$> get

getEdgeLabelIndexMap :: GS LabelIndexMap
getEdgeLabelIndexMap = snd . labelIndexMaps <$> get

getEdgeLabelMap :: GS LabelMap
getEdgeLabelMap = snd . labelMaps <$> get


getNodeLabelIndex :: Label -> GS (Maybe LabelIndex)
getNodeLabelIndex l = M.lookup l <$> getNodeLabelMap

getNodeLabel :: LabelIndex -> GS (Maybe Label)
getNodeLabel li = M.lookup li <$> getNodeLabelIndexMap

getEdgeLabelIndex :: Label -> GS (Maybe LabelIndex)
getEdgeLabelIndex l = M.lookup l <$> getEdgeLabelMap

getEdgeLabel :: LabelIndex -> GS (Maybe Label)
getEdgeLabel li = M.lookup li <$> getEdgeLabelIndexMap


addLabel :: (LabelIndexMap, LabelMap) -> LabelIndex -> Label -> (LabelIndexMap, LabelMap)
addLabel (lim, lm) li l = (M.insert li l lim, M.insert l li lm)


createNodeLabel' :: Label -> Graph -> (LabelIndex, Graph)
createNodeLabel' l g = maybe (nextIndex, result) (\li -> (li, g)) labelIndex
    where
        (nodeLabelIndexMap, edgeLabelIndexMap)  = labelIndexMaps g
        (nodeLabelMap, edgeLabelMap)            = labelMaps g
        labelIndex                              = M.lookup l nodeLabelMap
        nextIndex                               = nextNodeLabelIndex $ graphConfig g
        (newNodeLabelIndexMap, newNodeLabelMap) = addLabel (nodeLabelIndexMap, nodeLabelMap) nextIndex l
        result1                                 = incrementNodeLabelIndex
        result2                                 = alterLabelIndexMaps (newNodeLabelIndexMap, edgeLabelIndexMap) >>= const result1
        result3                                 = alterLabelMaps (newNodeLabelMap, edgeLabelMap) >>= const result2
        result                                  = execState result3 g

createEdgeLabel' :: Label -> Graph -> (LabelIndex, Graph)
createEdgeLabel' l g = maybe (nextIndex, result) (\li -> (li, g)) labelIndex
    where
        (nodeLabelIndexMap, edgeLabelIndexMap)  = labelIndexMaps g
        (nodeLabelMap, edgeLabelMap)            = labelMaps g
        labelIndex                              = M.lookup l edgeLabelMap
        nextIndex                               = nextEdgeLabelIndex $ graphConfig g
        (newEdgeLabelIndexMap, newEdgeLabelMap) = addLabel (edgeLabelIndexMap, edgeLabelMap) nextIndex l
        result1                                 = incrementEdgeLabelIndex
        result2                                 = alterLabelIndexMaps (nodeLabelIndexMap, newEdgeLabelIndexMap) >>= const result1
        result3                                 = alterLabelMaps (nodeLabelMap, newEdgeLabelMap) >>= const result2
        result                                  = execState result3 g

createNodeLabel :: Label -> GS LabelIndex
createNodeLabel l = state $ createNodeLabel' l

createEdgeLabel :: Label -> GS LabelIndex
createEdgeLabel l = state $ createEdgeLabel' l


createNodeLabels :: S.Set Label -> GS (S.Set LabelIndex)
createNodeLabels ls = state $ \g -> S.foldl aux (S.empty, g) ls
    where
        aux (olis, og) nl = (S.insert nli olis, ng)
            where
                (nli, ng) = createNodeLabel' nl og

createEdgeLabels :: S.Set Label -> GS (S.Set LabelIndex)
createEdgeLabels ls = state $ \g -> S.foldl aux (S.empty, g) ls
    where
        aux (olis, og) nl = (S.insert nli olis, ng)
            where
                (nli, ng) = createEdgeLabel' nl og


isNodeLabel :: Label -> GS Bool
isNodeLabel l = M.member l . fst . labelMaps <$> get

isEdgeLabel :: Label -> GS Bool
isEdgeLabel l = M.member l . snd . labelMaps <$> get

getNodeLabelIndexFilter :: (Label -> Bool) -> LabelIndex -> GS Bool
getNodeLabelIndexFilter f li = do l <- getNodeLabel li
                                  return $ maybe False f l

getEdgeLabelIndexFilter :: (Label -> Bool) -> LabelIndex -> GS Bool
getEdgeLabelIndexFilter f li = do l <- getEdgeLabel li
                                  return $ maybe False f l
