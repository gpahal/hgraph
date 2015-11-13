module HGraph.SocialNetwork where

import           Control.Applicative
import           Control.Arrow
import           Control.Monad.State
import qualified Data.Map             as M
import qualified Data.Maybe           as MB
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

data IdTree = IdTree Id [IdTree]
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
createPage ui title = do pin <- createNodeWithLabel pageLabel
                         _ <- setNodeProperties [(pageTitle, toValue $ toText title)] pin
                         ei <- createEdge createdLabel ui pin
                         return (pin, ei)

-- create edges
areConnected :: Id -> Id -> Label -> GS Bool
areConnected si ei l = do es <- getOutNodes (==l) si
                          let ess = map (nodeId . snd) es
                          return $ ei `elem` ess


addFriendW :: IntValue a => Id -> Id -> a -> GS ()
addFriendW i1 i2 w = do n1 <- getNodeByIdUnsafe i1
                        n2 <- getNodeByIdUnsafe i2
                        b1 <- areConnected (nodeId n1) (nodeId n2) friendLabel
                        b2 <- hasNodeLabelN userLabel n1
                        b3 <- hasNodeLabelN userLabel n2
                        unless b1 $ do
                            (e1, e2, _, _) <- if b2 && b3
                                then createEdgeNPair friendLabel n1 n2
                                else error "incorrect ids in function for adding friends"
                            let wv = toValue $ toInt w
                            _ <- setEdgePropertyE friendWeight wv e1
                            _ <- setEdgePropertyE friendWeight wv e2
                            return ()

addFriend :: Id -> Id -> GS ()
addFriend i1 i2 = addFriendW i1 i2 (1 :: Int)

likePage :: Id -> Id -> GS ()
likePage i1 i2 = do un <- getNodeByIdUnsafe i1
                    pn <- getNodeByIdUnsafe i2
                    b1 <- areConnected (nodeId un) (nodeId pn) likesLabel
                    b2 <- hasNodeLabelN userLabel un
                    b3 <- hasNodeLabelN pageLabel pn
                    unless b1 $ do
                        (_, _, _) <- if b2 && b3
                            then createEdgeN likesLabel un pn
                            else error "incorrect ids in function for liking pages"
                        return ()

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
pages i = listToSet <$> outEdgesHelper userLabel [createdLabel] i

users :: Id -> GS (S.Set Id)
users i = listToSet <$> inEdgesHelper pageLabel [likesLabel] i

creator :: Id -> GS Id
creator i = (snd . head) <$> inEdgesHelper pageLabel [createdLabel] i

userToPages :: Id -> GS (S.Set Id)
userToPages i = listToSet <$> outEdgesHelper userLabel [likesLabel, createdLabel] i

pageToUsers :: Id -> GS (S.Set Id)
pageToUsers i = listToSet <$> inEdgesHelper pageLabel [likesLabel, createdLabel] i

pathToNodeList :: Path -> [Node]
pathToNodeList (Path n l) = n:map snd l

pathToIdList :: Path -> [Id]
pathToIdList (Path n l) = nodeId n:map (nodeId . snd) l

pathTreeToNodeTree :: PathTree -> NodeTree
pathTreeToNodeTree (PathTree n l) = NodeTree n (map (\(_, pt) -> pathTreeToNodeTree pt) l)

pathTreeToIdTree :: PathTree -> IdTree
pathTreeToIdTree (PathTree n l) = IdTree (nodeId n) (map (\(_, pt) -> pathTreeToIdTree pt) l)

getWeight :: Edge -> Int
getWeight e = fromInt $ intVal val
    where
        val             = MB.fromMaybe (VInt 1) $ getEdgePropertySE friendWeight e
        intVal (VInt v) = v
        intVal _        = 1

djikstraLabelsRN :: [Label] -> Id -> Id -> GS [(Int, [Node])]
djikstraLabelsRN ls si ti = do res <- dijkstra 3 5 DOUT ((==ti) . nodeId) ls getWeight si
                               return $ map (second pathToNodeList) res

djikstraTreeLabelsRN :: [Label] -> Id -> Id -> GS NodeTree
djikstraTreeLabelsRN ls si ti = do res <- dijkstraTree 3 5 DOUT ((==ti) . nodeId) ls getWeight si
                                   return $ pathTreeToNodeTree res

djikstraLabels :: [Label] -> Id -> Id -> GS [(Int, [Id])]
djikstraLabels ls si ti = do res <- dijkstra 3 5 DOUT ((==ti) . nodeId) ls getWeight si
                             return $ map (second pathToIdList) res

djikstraTreeLabels :: [Label] -> Id -> Id -> GS IdTree
djikstraTreeLabels ls si ti = do res <- dijkstraTree 3 5 DOUT ((==ti) . nodeId) ls getWeight si
                                 return $ pathTreeToIdTree res

djikstraUserRN :: Id -> Id -> GS [(Int, [Node])]
djikstraUserRN = djikstraLabelsRN [friendLabel]

djikstraTreeUserRN :: Id -> Id -> GS NodeTree
djikstraTreeUserRN = djikstraTreeLabelsRN [friendLabel]

djikstraPageRN :: Id -> Id -> GS [(Int, [Node])]
djikstraPageRN = djikstraLabelsRN [friendLabel, likesLabel, createdLabel]

djikstraTreePageRN :: Id -> Id -> GS NodeTree
djikstraTreePageRN = djikstraTreeLabelsRN [friendLabel, likesLabel, createdLabel]

djikstraUser :: Id -> Id -> GS [(Int, [Id])]
djikstraUser = djikstraLabels [friendLabel]

djikstraTreeUser :: Id -> Id -> GS IdTree
djikstraTreeUser = djikstraTreeLabels [friendLabel]

djikstraPage :: Id -> Id -> GS [(Int, [Id])]
djikstraPage = djikstraLabels [friendLabel, likesLabel, createdLabel]

djikstraTreePage :: Id -> Id -> GS IdTree
djikstraTreePage = djikstraTreeLabels [friendLabel, likesLabel, createdLabel]

commonNeighbors :: Label -> Label -> [Label] -> Id -> Id -> GS (S.Set Id)
commonNeighbors l1 l2 ls i1 i2 = do res1 <- edgesHelper DOUT l1 ls i1
                                    res2 <- edgesHelper DOUT l2 ls i2
                                    let ns1 = listToSet res1
                                    let ns2 = listToSet res2
                                    return $ S.intersection ns1 ns2

mutualFriends :: Id -> Id -> GS (S.Set Id)
mutualFriends = commonNeighbors userLabel userLabel [friendLabel]

commonPages :: Id -> Id -> GS (S.Set Id)
commonPages = commonNeighbors userLabel userLabel [likesLabel, createdLabel]

mutualScore :: Id -> Id -> GS (S.Set Id)
mutualScore = commonNeighbors userLabel userLabel [friendLabel, likesLabel, createdLabel]

-- recommendations
setToMap :: Ord a => S.Set a -> M.Map a Int
setToMap = S.foldl (\acc v -> M.insert v 1 acc) M.empty

setsToMaps :: Ord a => [S.Set a] -> [M.Map a Int]
setsToMaps = map setToMap

userRecommendations :: Id -> GS (M.Map Id Int)
userRecommendations i = do fs <- friends i
                           ls <- userToPages i
                           fofs <- mapM friends $ S.toList fs
                           uols <- mapM pageToUsers $ S.toList ls
                           return $ M.difference
                                (M.unionWith (+) (foldl (M.unionWith (+)) M.empty $ setsToMaps fofs)
                                (foldl (M.unionWith (+)) M.empty $ setsToMaps uols))
                                $ M.insert i 1 $ setToMap fs

pageRecommendations :: Id -> GS (M.Map Id Int)
pageRecommendations i = do ls <- userToPages i
                           fs <- friends i
                           fls <- mapM userToPages $ S.toList fs
                           fofst <- mapM friends $ S.toList fs
                           let fofs = S.difference (S.foldl S.union S.empty $ S.fromList fofst) fs
                           fofls <- mapM userToPages $ S.toList fofs
                           let lsm = setToMap ls
                           let diff1 = M.difference (foldl (M.unionWith (+)) M.empty $ setsToMaps fls) lsm
                           let diff2 = M.difference (foldl (M.unionWith (+)) M.empty $ setsToMaps fofls) lsm
                           let ppr = if M.size diff1 > 2 then diff1 else M.unionWith (+) diff1 diff2
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

randPerm :: StdGen -> [a] -> [a]
randPerm _ []   = []
randPerm g xs = let (n, ng) = randomR (0,length xs - 1) g
                    front = xs !! n
                in  front : randPerm ng (take n xs ++ drop (n+1) xs)

createRandomPages :: [Id] -> Int -> GS [(Id, Id)]
createRandomPages is c
    | c <= 0    = return []
    | l >= c    = do let mis = aux fis (1 :: Int)
                     mapM (\(v, i) -> createPage v ("page " ++ show i)) mis
    | otherwise = error "number of pages must be less than number of users"
    where
        l   = length is
        fis = randPerm (mkStdGen 13) is
        aux []     _ = []
        aux (x:xs) i = (x, i):aux xs (i+1)

addFriends :: Id -> [Id] -> GS ()
addFriends _ []     = return ()
addFriends i (x:xs) = unless (x == i) $ do
                        _ <- addFriend i x
                        _ <- addFriends i xs
                        return ()

createRandomFriends :: [Id] -> Int -> GS ()
createRandomFriends is c
    | c <= 0    = return ()
    | l >= c    = do _ <- mapM (\v -> addFriends v $ aux v) is
                     return ()
    | otherwise = error "number of friends must be less than number of users"
    where
        l = length is
        aux x = take c $ randPerm (mkStdGen $ fromInt x) is

likePages :: Id -> [Id] -> GS ()
likePages _ []     = return ()
likePages i (x:xs) = do v <- likePage i x
                        vs <- likePages i xs
                        return ()

createRandomLikes :: [Id] -> [Id] -> Int -> GS ()
createRandomLikes uis pis c
    | c <= 0    = return ()
    | l >= c    = do _ <- mapM (\v -> likePages v $ aux v) uis
                     return ()
    | otherwise = error "number of likes must be less than number of pages"
    where
        l = length pis
        aux x = take c $ randPerm (mkStdGen $ fromInt x) pis

createRandomGraph :: Int -> Int -> Int -> Int -> GS ()
createRandomGraph cn cp cf cl = do us <- createRandomUsers cn
                                   ps <- createRandomPages us cp
                                   _ <- createRandomFriends us cf
                                   _ <- createRandomLikes us (map fst ps) cl
                                   return ()
