module HGraph.Graph where

import           Control.Applicative
import           Control.Monad.State
import qualified Data.ByteString     as BS
import qualified Data.Map            as M
import           Data.Maybe
import qualified Data.Serialize      as Se
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

alterRemoveLabelInstance :: LabelInstances -> LabelIndex -> Node -> GS ()
alterRemoveLabelInstance lins li n = alterLabelInstances $ M.alter aux li lins
    where
        i            = nodeId n
        aux Nothing  = Nothing
        aux (Just x) = Just $ S.delete i x

alterRemoveLabelInstances :: LabelInstances -> S.Set LabelIndex -> Node -> GS ()
alterRemoveLabelInstances lins lis n = S.foldl aux (return ()) lis
    where
        aux _ li = alterRemoveLabelInstance lins li n

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

getNodeLabels :: GS [Label]
getNodeLabels = do g <- get
                   return $ M.keys $ fst $ labelMaps g

getEdgeLabels :: GS [Label]
getEdgeLabels = do g <- get
                   return $ M.keys $ snd $ labelMaps g

getNodeCount :: GS Int
getNodeCount = do g <- get
                  return $ M.size $ nodes g

getEdgeCount :: GS Int
getEdgeCount = do g <- get
                  return $ M.size $ edges g

getNodeById :: Id -> GS (Maybe Node)
getNodeById i = M.lookup i . nodes <$> get

getNodeByIdUnsafe :: Id -> GS Node
getNodeByIdUnsafe i = fromJust . M.lookup i . nodes <$> get

getEdgeById :: Id -> GS (Maybe Edge)
getEdgeById i = M.lookup i . edges <$> get

getEdgeByIdUnsafe :: Id -> GS Edge
getEdgeByIdUnsafe i = fromJust . M.lookup i . edges <$> get

saveToDisk :: Graph -> FilePath -> IO ()
saveToDisk g fp = BS.writeFile fp $ Se.encode g

loadFromDisk :: FilePath -> IO Graph
loadFromDisk fp = do eg <- Se.decode <$> BS.readFile fp
                     return $ either (error "error reading from disk") id eg
