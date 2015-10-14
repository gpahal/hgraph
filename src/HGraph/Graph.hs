module HGraph.Graph where

import HGraph.Types
import HGraph.GraphConfig

alterGraphConfig :: Graph -> GraphConfig -> Graph
alterGraphConfig g gc = Graph gc (labelIndexMaps g) (labelMaps g) (labelInstances g) (nodes g) (edges g)

alterLabelIndexMaps :: Graph -> LabelIndexMaps -> Graph
alterLabelIndexMaps g lim = Graph (graphConfig g) lim (labelMaps g) (labelInstances g) (nodes g) (edges g)

alterLabelMaps :: Graph -> LabelMaps -> Graph
alterLabelMaps g lm = Graph (graphConfig g) (labelIndexMaps g) lm (labelInstances g) (nodes g) (edges g)

alterLabelInstances :: Graph -> LabelInstances -> Graph
alterLabelInstances g li = Graph (graphConfig g) (labelIndexMaps g) (labelMaps g) li (nodes g) (edges g)

alterLabelNodes :: Graph -> Nodes -> Graph
alterLabelNodes g n = Graph (graphConfig g) (labelIndexMaps g) (labelMaps g) (labelInstances g) n (edges g)

alterLabelEdges :: Graph -> Edges -> Graph
alterLabelEdges g = Graph (graphConfig g) (labelIndexMaps g) (labelMaps g) (labelInstances g) (nodes g)

incrementNodeLabelIndex :: Graph -> Graph
incrementNodeLabelIndex g = alterGraphConfig g $ incrementNodeLabelIndex' $ graphConfig g

incrementEdgeLabelIndex :: Graph -> Graph
incrementEdgeLabelIndex g = alterGraphConfig g $ incrementEdgeLabelIndex' $ graphConfig g

incrementNodeId :: Graph -> Graph
incrementNodeId g = alterGraphConfig g $ incrementNodeId' $ graphConfig g

incrementEdgeId :: Graph -> Graph
incrementEdgeId g = alterGraphConfig g $ incrementEdgeId' $ graphConfig g