module HGraph.Label where

import HGraph.Types
import HGraph.Graph
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


getLabelIndex' :: LabelMap -> Label -> Maybe LabelIndex
getLabelIndex' lm l = Map.lookup l lm

getLabel' :: LabelIndexMap -> LabelIndex -> Maybe Label
getLabel' lim li = Map.lookup li lim


getNodeLabelIndex :: Graph -> Label -> Maybe LabelIndex
getNodeLabelIndex = getLabelIndex' . getNodeLabelMap

getNodeLabel :: Graph -> LabelIndex -> Maybe Label
getNodeLabel = getLabel' . getNodeLabelIndexMap

getEdgeLabelIndex :: Graph -> Label -> Maybe LabelIndex
getEdgeLabelIndex = getLabelIndex' . getEdgeLabelMap

getEdgeLabel :: Graph -> LabelIndex -> Maybe Label
getEdgeLabel = getLabel' . getEdgeLabelIndexMap


addLabel :: (LabelIndexMap, LabelMap) -> LabelIndex -> Label -> (LabelIndexMap, LabelMap)
addLabel (lim, lm) li l = (Map.insert li l lim, Map.insert l li lm)


createNodeLabel :: Graph -> Label -> (Graph, LabelIndex)
createNodeLabel g l = maybe (result, nextIndex) (\li -> (g, li)) nodeLabelIndex
    where
        (nodeLabelIndexMap, edgeLabelIndexMap)  = labelIndexMaps g
        (nodeLabelMap, edgeLabelMap)            = labelMaps g
        nodeLabelIndex                          = getLabelIndex' nodeLabelMap l
        nextIndex                               = nextNodeLabelIndex $ graphConfig g
        (newNodeLabelIndexMap, newNodeLabelMap) = addLabel (nodeLabelIndexMap, nodeLabelMap) nextIndex l
        result1                                 = incrementNodeLabelIndex g
        result2                                 = alterLabelIndexMaps result1 (newNodeLabelIndexMap, edgeLabelIndexMap)
        result                                  = alterLabelMaps result2 (newNodeLabelMap, edgeLabelMap)

createEdgeLabel :: Graph -> Label -> (Graph, LabelIndex)
createEdgeLabel g l = maybe (result, nextIndex) (\li -> (g, li)) edgeLabelIndex
    where
        (nodeLabelIndexMap, edgeLabelIndexMap)  = labelIndexMaps g
        (nodeLabelMap, edgeLabelMap)            = labelMaps g
        edgeLabelIndex                          = getLabelIndex' edgeLabelMap l
        nextIndex                               = nextEdgeLabelIndex $ graphConfig g
        (newEdgeLabelIndexMap, newEdgeLabelMap) = addLabel (edgeLabelIndexMap, edgeLabelMap) nextIndex l
        result1                                 = incrementEdgeLabelIndex g
        result2                                 = alterLabelIndexMaps result1 (nodeLabelIndexMap, newEdgeLabelIndexMap)
        result                                  = alterLabelMaps result2 (nodeLabelMap, newEdgeLabelMap)


isNodeLabel :: Graph -> Label -> Bool
isNodeLabel g l = Maybe.isJust nodeLabelIndex
    where
        (nodeLabelMap, _) = labelMaps g
        nodeLabelIndex    = getLabelIndex' nodeLabelMap l

isEdgeLabel :: Graph -> Label -> Bool
isEdgeLabel g l = Maybe.isJust edgeLabelIndex
    where
        (_, edgeLabelMap) = labelMaps g
        edgeLabelIndex    = getLabelIndex' edgeLabelMap l