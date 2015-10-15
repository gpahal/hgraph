module HGraph.Graph where

import           Control.Monad.State
import qualified Data.Map            as M
import qualified Data.Set            as S
import           HGraph.GraphConfig
import           HGraph.Types

emptyGraph :: Graph
emptyGraph = Graph emptyGraphConfig (M.empty, M.empty) (M.empty, M.empty) M.empty M.empty M.empty


alterGraphConfig :: GraphConfig -> GS ()
alterGraphConfig gc = modify (\g -> Graph gc (labelIndexMaps g) (labelMaps g) (nodeLabelInstances g) (nodes g) (edges g))

alterLabelIndexMaps :: LabelIndexMaps -> GS ()
alterLabelIndexMaps lim = modify (\g -> Graph (graphConfig g) lim (labelMaps g) (nodeLabelInstances g) (nodes g) (edges g))

alterLabelMaps :: LabelMaps -> GS ()
alterLabelMaps lm = modify (\g -> Graph (graphConfig g) (labelIndexMaps g) lm (nodeLabelInstances g) (nodes g) (edges g))

alterLabelInstances :: LabelInstances -> GS ()
alterLabelInstances li = modify (\g-> Graph (graphConfig g) (labelIndexMaps g) (labelMaps g) li (nodes g) (edges g))

alterAddLabelInstance :: LabelInstances -> LabelIndex -> Node -> GS ()
alterAddLabelInstance lins li n = alterLabelInstances $ M.alter aux li lins
    where
        i            = nodeId n
        aux Nothing  = Just $ S.singleton i
        aux (Just x) = Just $ S.insert i x

alterAddLabelInstances :: LabelInstances -> S.Set LabelIndex -> Node -> GS ()
alterAddLabelInstances lins lis n = S.foldl aux (return ()) lis
    where
        aux _ li = alterAddLabelInstance lins li n

alterNodes :: Nodes -> GS ()
alterNodes n = modify (\g -> Graph (graphConfig g) (labelIndexMaps g) (labelMaps g) (nodeLabelInstances g) n (edges g))

alterEdges :: Edges -> GS ()
alterEdges e = modify (\g -> Graph (graphConfig g) (labelIndexMaps g) (labelMaps g) (nodeLabelInstances g) (nodes g) e)

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
