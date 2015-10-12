module HGraph.Graph where

import HGraph.Types

alterLabelIndexMaps :: Graph -> LabelIndexMaps -> Graph
alterLabelIndexMaps g lim = Graph lim (labelMaps g) (labelInstances g) (nodes g) (edges g)

alterLabelMaps :: Graph -> LabelMaps -> Graph
alterLabelMaps g lm = Graph (labelIndexMaps g) lm (labelInstances g) (nodes g) (edges g)

alterLabelInstances :: Graph -> LabelInstances -> Graph
alterLabelInstances g li = Graph (labelIndexMaps g) (labelMaps g) li (nodes g) (edges g)

alterLabelNodes :: Graph -> Nodes -> Graph
alterLabelNodes g n = Graph (labelIndexMaps g) (labelMaps g) (labelInstances g) n (edges g)

alterLabelEdges :: Graph -> Edges -> Graph
alterLabelEdges g = Graph (labelIndexMaps g) (labelMaps g) (labelInstances g) (nodes g)