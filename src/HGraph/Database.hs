module HGraph.Database where

import qualified Data.ByteString.Char8               as BS
import qualified Data.HashMap.Strict                 as HM
import qualified Data.Map                            as M
import qualified Data.Maybe                          as MB
import qualified Data.Text                           as T
import qualified Data.Traversable                    as Tr
import qualified Database.Neo4j                      as N
import qualified Database.Neo4j.Batch                as B
import qualified Database.Neo4j.Graph                as NG
import qualified Database.Neo4j.Transactional.Cypher as NC
import qualified Database.Neo4j.Types                as NT
import           HGraph.Edge
import           HGraph.Node
import           HGraph.Types

keyToText :: Key -> T.Text
keyToText = id

valueToPV :: Value -> NT.PropertyValue
valueToPV v = NT.ValueProperty $ NT.TextVal v

idToPV :: Id -> NT.PropertyValue
idToPV v = NT.ValueProperty $ NT.IntVal v

nodeToProperties :: Node -> NT.Properties
nodeToProperties n = HM.insert (keyToText idTextKey) (idToPV $ nodeId n) $ M.foldlWithKey (\a k v -> HM.insert (keyToText k) (valueToPV v) a) HM.empty $ nodeProperties n

edgeToProperties :: Edge -> NT.Properties
edgeToProperties e = HM.insert (keyToText idTextKey) (idToPV $ edgeId e) $ M.foldlWithKey (\a k v -> HM.insert (keyToText k) (valueToPV v) a) HM.empty $ edgeProperties e

errorHelper :: Maybe a -> String -> a
errorHelper ma s = MB.fromMaybe (error $ "error in function " ++ s) ma

saveGraphToDatabase :: Graph -> IO NG.Graph
saveGraphToDatabase g = N.withAuthConnection (BS.pack "127.0.0.1") 7474 (BS.pack "neo4j", BS.pack "neo4j") $ do
    _ <- NC.runTransaction $ NC.cypher (T.pack "MATCH (n) DETACH DELETE n") HM.empty
    B.runBatch $ do
        let ns = nodes g
        let es = edges g
        let ls = M.map (nodeLabelsS g) ns
        dbNodes <- Tr.mapM (B.createNode . nodeToProperties) ns
        let ehf1 x = errorHelper (M.lookup x ls) "saveGraphToDatabase"
        let ehf2 x = errorHelper (M.lookup x dbNodes) "saveGraphToDatabase"
        mapM_ (\k -> B.addLabels (ehf1 k) (ehf2 k)) $ M.keys dbNodes
        Tr.mapM (\e -> let (sn, en) = connection e in B.createRelationship (edgeLabelS g e) (edgeToProperties e) (ehf2 sn) (ehf2 en)) es
