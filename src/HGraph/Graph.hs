module HGraph.Graph where

import           Control.Monad.State
import           HGraph.GraphConfig
import           HGraph.Types

alterGraphConfig :: GraphConfig -> GS ()
alterGraphConfig gc = modify (\g -> Graph gc (labelIndexMaps g) (labelMaps g) (labelInstances g) (nodes g) (edges g))

alterLabelIndexMaps :: LabelIndexMaps -> GS ()
alterLabelIndexMaps lim = modify (\g -> Graph (graphConfig g) lim (labelMaps g) (labelInstances g) (nodes g) (edges g))

alterLabelMaps :: LabelMaps -> GS ()
alterLabelMaps lm = modify (\g -> Graph (graphConfig g) (labelIndexMaps g) lm (labelInstances g) (nodes g) (edges g))

alterLabelInstances :: LabelInstances -> GS ()
alterLabelInstances li = modify (\g-> Graph (graphConfig g) (labelIndexMaps g) (labelMaps g) li (nodes g) (edges g))

alterNodes :: Nodes -> GS ()
alterNodes n = modify (\g -> Graph (graphConfig g) (labelIndexMaps g) (labelMaps g) (labelInstances g) n (edges g))

alterEdges :: Edges -> GS ()
alterEdges e = modify (\g -> Graph (graphConfig g) (labelIndexMaps g) (labelMaps g) (labelInstances g) (nodes g) e)

incrementNodeLabelIndex :: GS LabelIndex
incrementNodeLabelIndex = do g <- get
                             let li = nextNodeLabelIndex $ graphConfig g
                             alterGraphConfig $ incrementNodeLabelIndex' $ graphConfig g
                             return li

incrementEdgeLabelIndex :: GS LabelIndex
incrementEdgeLabelIndex = do g <- get
                             let li = nextNodeLabelIndex $ graphConfig g
                             alterGraphConfig $ incrementEdgeLabelIndex' $ graphConfig g
                             return li

incrementNodeId :: GS Id
incrementNodeId = do g <- get
                     let i = nextNodeId $ graphConfig g
                     alterGraphConfig $ incrementNodeId' $ graphConfig g
                     return i

incrementEdgeId :: GS Id
incrementEdgeId = do g <- get
                     let i = nextEdgeId $ graphConfig g
                     alterGraphConfig $ incrementEdgeId' $ graphConfig g
                     return i
