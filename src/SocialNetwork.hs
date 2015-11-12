module SocialNetwork where

import           Control.Applicative
import           Control.Arrow
import           Control.Monad.State
import qualified Data.Map             as M
import qualified Data.Maybe           as MB
import           Data.Random
import qualified Data.Set             as S
import qualified Data.Text            as T
import qualified Data.Traversable     as Tr
import qualified Database.Neo4j.Graph as NG
import           HGraph.Database
import           HGraph.Edge
import           HGraph.Graph
import           HGraph.GraphConfig
import           HGraph.Label
import           HGraph.Node
import           HGraph.Path
import           HGraph.Query
import           HGraph.Types
import           System.Random

data NodeTree = NodeTree Node [NodeTree]
                deriving (Eq, Show)

-- Node labels
userLabel :: Label
userLabel = toText "User"

pageLabel :: Label
pageLabel = toText "Page"

-- Edge labels
friendLabel :: Label
friendLabel = toText "Friend"

createdLabel :: Label
createdLabel = toText "Created"

likesLabel :: Label
likesLabel = toText "Likes"

-- Node fields
userName :: Key
userName = toText "name"

userAge :: Key
userAge = toText "age"

pageTitle :: Key
pageTitle = toText "title"

-- Edge fields
friendWeight :: Key
friendWeight = toText "weight"

-- create functions
createUser :: (TextValue a, IntValue b) => a -> b -> GS Id
createUser name age = do i <- createNodeWithLabel userLabel
                         _ <- setNodeProperties [(userName, toValue $ toText name), (userAge, toValue $ toInt age)] i
                         return i

createPage :: (TextValue a) => Id -> a -> GS (Id, Id)
createPage ui title = do pi <- createNodeWithLabel pageLabel
                         _ <- setNodeProperties [(pageTitle, toValue $ toText title)] pi
                         ei <- createEdge createdLabel ui pi
                         return (pi, ei)

-- create edges
addFriendW :: IntValue a => Id -> Id -> a -> GS (Id, Id)
addFriendW i1 i2 w = do n1 <- getNodeByIdUnsafe i1
                        n2 <- getNodeByIdUnsafe i2
                        b1 <- hasNodeLabelN userLabel n1
                        b2 <- hasNodeLabelN userLabel n2
                        (e1, e2, _, _) <- if b1 && b2
                            then createEdgeNPair friendLabel n1 n2
                            else error "incorrect ids in function for adding friends"
                        let wv = toValue $ toInt w
                        _ <- setEdgePropertyE friendWeight wv e1
                        _ <- setEdgePropertyE friendWeight wv e2
                        return (edgeId e1, edgeId e2)

addFriend :: Id -> Id -> GS (Id, Id)
addFriend i1 i2 = addFriendW i1 i2 (1 :: Int)

likePage :: Id -> Id -> GS Id
likePage i1 i2 = do un <- getNodeByIdUnsafe i1
                    pn <- getNodeByIdUnsafe i2
                    b1 <- hasNodeLabelN userLabel un
                    b2 <- hasNodeLabelN pageLabel pn
                    (e, _, _) <- if b1 && b2
                        then createEdgeN likesLabel un pn
                        else error "incorrect ids in function for liking pages"
                    return $ edgeId e

-- query functions
edgesHelper :: Direction -> Label -> [Label] -> Id -> GS [(Id, Id)]
edgesHelper d sl els i = do n <- getNodeByIdUnsafe i
                            b <- hasNodeLabelN sl n
                            let aux = (`elem` els)
                            on <- case d of
                                DOUT -> getOutNodes aux i
                                DIN -> getInNodes aux i
                                DBOTH -> getNodes aux i
                            if b
                                then return $ map (edgeId *** nodeId) on
                                else error "incorrect id in function for retrieving friends"

outEdgesHelper :: Label -> [Label] -> Id -> GS [(Id, Id)]
outEdgesHelper = edgesHelper DOUT

inEdgesHelper :: Label -> [Label] -> Id -> GS [(Id, Id)]
inEdgesHelper = edgesHelper DIN

bothEdgesHelper :: Label -> [Label] -> Id -> GS [(Id, Id)]
bothEdgesHelper = edgesHelper DBOTH

listToSet :: Ord b => [(a, b)] -> S.Set b
listToSet l = S.fromList $ map snd l

friends :: Id -> GS (S.Set Id)
friends i = listToSet <$> outEdgesHelper userLabel [friendLabel] i

likes :: Id -> GS (S.Set Id)
likes i = listToSet <$> outEdgesHelper userLabel [likesLabel] i

pages :: Id -> GS (S.Set Id)
pages i = listToSet <$> inEdgesHelper userLabel [createdLabel] i

users :: Id -> GS (S.Set Id)
users i = listToSet <$> inEdgesHelper pageLabel [likesLabel] i

creator :: Id -> GS Id
creator i = (snd . head) <$> inEdgesHelper pageLabel [createdLabel] i

pathToNodeList :: Path -> [Node]
pathToNodeList (Path n l) = n:map snd l

pathTreeToNodeTree :: PathTree -> NodeTree
pathTreeToNodeTree (PathTree n l) = NodeTree n (map (\(_, pt) -> pathTreeToNodeTree pt) l)

getWeight :: Edge -> Int
getWeight e = fromInt $ intVal val
    where
        val             = MB.fromMaybe (VInt 1) $ getEdgePropertySE friendWeight e
        intVal (VInt v) = v
        intVal _        = 1

djikstraLabels :: [Label] -> Id -> Id -> GS [(Int, [Node])]
djikstraLabels ls si ti = do res <- dijkstra 4 3 DOUT ((==ti) . nodeId) ls getWeight si
                             return $ map (second pathToNodeList) res

djikstraTreeLabels :: [Label] -> Id -> Id -> GS NodeTree
djikstraTreeLabels ls si ti = do res <- dijkstraTree 4 3 DOUT ((==ti) . nodeId) ls getWeight si
                                 return $ pathTreeToNodeTree res

djikstraUser :: Id -> Id -> GS [(Int, [Node])]
djikstraUser = djikstraLabels [userLabel]

djikstraPage :: Id -> Id -> GS [(Int, [Node])]
djikstraPage = djikstraLabels [userLabel, pageLabel]

commonNeighbors :: Label -> Label -> [Label] -> Id -> Id -> GS (S.Set Id)
commonNeighbors l1 l2 ls i1 i2 = do res1 <- edgesHelper DOUT l1 ls i1
                                    res2 <- edgesHelper DOUT l2 ls i2
                                    let ns1 = listToSet res1
                                    let ns2 = listToSet res2
                                    return $ S.intersection ns1 ns2

mutualFriends :: Id -> Id -> GS (S.Set Id)
mutualFriends = commonNeighbors userLabel userLabel [friendLabel]

commonLikes :: Id -> Id -> GS (S.Set Id)
commonLikes = commonNeighbors userLabel userLabel [likesLabel]

mutualScore :: Id -> Id -> GS (S.Set Id)
mutualScore = commonNeighbors userLabel userLabel [friendLabel, likesLabel, createdLabel]

-- recommendations
setToMap :: Ord a => S.Set a -> M.Map a Int
setToMap = S.foldl (\acc v -> M.insert v 1 acc) M.empty

setsToMaps :: Ord a => [S.Set a] -> [M.Map a Int]
setsToMaps = map setToMap

userRecommendations :: Id -> GS (M.Map Id Int)
userRecommendations i = do fs <- friends i
                           ls <- likes i
                           fofs <- mapM friends $ S.toList fs
                           uols <- mapM users $ S.toList ls
                           return $ M.difference
                                (M.intersectionWith (+) (foldl (M.intersectionWith (+)) M.empty $ setsToMaps fofs)
                                (foldl (M.intersectionWith (+)) M.empty $ setsToMaps uols))
                                $ setToMap fs

pageRecommendations :: Id -> GS (M.Map Id Int)
pageRecommendations i = do ls <- likes i
                           fs <- friends i
                           fls <- mapM likes $ S.toList fs
                           fofst <- mapM friends $ S.toList fs
                           let fofs = S.difference (S.foldl S.intersection S.empty $ S.fromList fofst) fs
                           fofls <- mapM likes $ S.toList fofs
                           let lsm = setToMap ls
                           let diff1 = M.difference (foldl (M.intersectionWith (+)) M.empty $ setsToMaps fls) lsm
                           let diff2 = M.difference (foldl (M.intersectionWith (+)) M.empty $ setsToMaps fofls) lsm
                           let ppr = if M.size diff1 > 2 then diff1 else M.intersectionWith (+) diff1 diff2
                           return ppr

-- helper functions
initialGraph :: Graph
initialGraph = emptyGraph

runGraph :: GS a -> Graph -> (a, Graph)
runGraph = runState

evalGraph :: GS a -> Graph -> a
evalGraph = evalState

execGraph :: GS a -> Graph -> Graph
execGraph = execState

randomAgeHelper :: StdGen -> (Int, StdGen)
randomAgeHelper = randomR (18 :: Int, 40 :: Int)

createRandomUsers :: Int -> GS [Id]
createRandomUsers c = do let pairs = foldl aux (mkStdGen 6, []) [1..c]
                         mapM (uncurry createUser) $ snd pairs
    where
        aux (g, l) v = let (a, ng) = randomAgeHelper g in (ng, ("user " ++ show v, a):l)
