module HGraph.GraphConfig where

import           HGraph.Types

incrementNodeLabelIndex' :: GraphConfig -> GraphConfig
incrementNodeLabelIndex' gc = GraphConfig (nextNodeLabelIndex gc + 1) (nextEdgeLabelIndex gc) (nextNodeId gc) (nextEdgeId gc)

incrementEdgeLabelIndex' :: GraphConfig -> GraphConfig
incrementEdgeLabelIndex' gc = GraphConfig (nextNodeLabelIndex gc) (nextEdgeLabelIndex gc + 1) (nextNodeId gc) (nextEdgeId gc)

incrementNodeId' :: GraphConfig -> GraphConfig
incrementNodeId' gc = GraphConfig (nextNodeLabelIndex gc) (nextEdgeLabelIndex gc) (nextNodeId gc + 1) (nextEdgeId gc)

incrementEdgeId' :: GraphConfig -> GraphConfig
incrementEdgeId' gc = GraphConfig (nextNodeLabelIndex gc) (nextEdgeLabelIndex gc) (nextNodeId gc) (nextEdgeId gc + 1)
