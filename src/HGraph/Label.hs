module HGraph.Label where

import HGraph.Types
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

getNodeLabelIndexMap :: Graph -> LabelIndexMap
getNodeLabelIndexMap g = nodeLabelIndexMap
    where
        (nodeLabelIndexMap, _) = labelIndexMaps g

getNodeLabelMap :: Graph -> LabelMap
getNodeLabelMap g = nodeLabelMap
    where
        (nodeLabelMap, _) = labelMaps g

getEdgeLabelIndexMap :: Graph -> LabelIndexMap
getEdgeLabelIndexMap g = edgeLabelIndexMap
    where
        (_, edgeLabelIndexMap) = labelIndexMaps g

getEdgeLabelMap :: Graph -> LabelMap
getEdgeLabelMap g = edgeLabelMap
    where
        (_, edgeLabelMap) = labelMaps g


getNodeLabelIndex' :: LabelMap -> Label -> Maybe LabelIndex
getNodeLabelIndex' lm l = Map.lookup l lm

getNodeLabel' :: LabelIndexMap -> LabelIndex -> Maybe Label
getNodeLabel' lim li = Map.lookup li lim

getEdgeLabelIndex' :: LabelMap -> Label -> Maybe LabelIndex
getEdgeLabelIndex' lm l = Map.lookup l lm

getEdgeLabel' :: LabelIndexMap -> LabelIndex -> Maybe Label
getEdgeLabel' lim li = Map.lookup li lim


getNodeLabelIndex :: Graph -> Label -> Maybe LabelIndex
getNodeLabelIndex = getNodeLabelIndex' . getNodeLabelMap

getNodeLabel :: Graph -> LabelIndex -> Maybe Label
getNodeLabel = getNodeLabel' . getNodeLabelIndexMap

getEdgeLabelIndex :: Graph -> Label -> Maybe LabelIndex
getEdgeLabelIndex = getEdgeLabelIndex' . getEdgeLabelMap

getEdgeLabel :: Graph -> LabelIndex -> Maybe Label
getEdgeLabel = getEdgeLabel' . getEdgeLabelIndexMap
