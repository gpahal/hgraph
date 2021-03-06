module HGraph.Path where

import           Control.Applicative
import           Control.Monad.State
import qualified Data.Map             as M
import qualified Data.Maybe           as MB
import qualified Data.PQueue.Prio.Min as PQ
import qualified Data.Set             as S
import           HGraph.Edge
import           HGraph.Graph
import           HGraph.Query
import           HGraph.Types

type PathAncestors = M.Map Id (Maybe (Node, Edge))

getAncestor :: PathAncestors -> Node -> Maybe (Node, Edge)
getAncestor pa n = MB.fromJust $ M.lookup (nodeId n) pa

constructPath :: PathAncestors -> Node -> Path
constructPath pa n = aux n []
    where
        aux cn p
            | MB.isNothing lr = Path cn p
            | otherwise       = aux an ((e, cn):p)
            where
                lr      = getAncestor pa cn
                (an, e) = MB.fromJust lr

addPathToPathTree :: Path -> PathTree -> PathTree
addPathToPathTree (Path pn po) (PathTree ptn pto) = if pn == ptn then PathTree ptn (aux2 po pto) else PathTree ptn pto
    where
        aux2 []            x                          = x
        aux2 ((e, on):xs2) []                         = [(e, addPathToPathTree (Path on xs2) (PathTree on []))]
        aux2 ((e, on):xs2) ((te, PathTree tn to):xs1) = if tn == on
                                                                 then (te, addPathToPathTree (Path on xs2) (PathTree tn to)):xs1
                                                                 else (te, PathTree tn to):aux2 ((e, on):xs2) xs1

addPathToPathTree' :: (Ord a, Num a) => (a, Path) -> PathTree -> PathTree
addPathToPathTree' (_, p) = addPathToPathTree p

constructPathTree :: Node -> [Path] -> PathTree
constructPathTree n = foldr addPathToPathTree (PathTree n [])

constructPathTreeW :: (Ord a, Num a) => Node -> [(a, Path)] -> PathTree
constructPathTreeW n = foldr addPathToPathTree' (PathTree n [])

dijkstraG' :: (Ord a, Num a) => Graph -> Int -> Int -> Direction
                             -> (Node -> Bool) -> (Edge -> Bool) -> (Label -> Bool) -> (Edge -> a)
                             -> PQ.MinPQueue a (Node, Maybe Edge) -> PQ.MinPQueue a (Node, Maybe Edge) -> PathAncestors
                             -> [(a, Path)] -> [(a, Path)]
dijkstraG' g de count di nbf ebf lbf vf cq nq pa ps
    | de < 0 || count == 0 = ps
    | cqn && nqn           = ps
    | cqn                  = dijkstraG' g (de - 1) count di nbf ebf lbf vf nq PQ.empty pa ps
    | otherwise            = dijkstraG' g de nn di nbf ebf lbf vf ncq nnq npa nps
    where
        cqn                 = PQ.null cq
        nqn                 = PQ.null nq
        ((k, (v, me)), ncq) = PQ.deleteFindMin cq
        vi                  = nodeId v
        ncs                 = nbf v
        nn  = if ncs then count - 1 else count
        gsEdges
            | di == DOUT  = getFilteredOutNodesN ebf lbf v
            | di == DIN   = getFilteredInNodesN ebf lbf v
            | di == DBOTH = combineGS (++) (getFilteredInNodesN ebf lbf v) (getFilteredOutNodesN ebf lbf v)
            | otherwise   = return []
        es = filter (\(_, n) -> not $ (nodeId n == vi) || M.member (nodeId n) pa) $ evalState gsEdges g
        nnq = if ncs then nq else foldl (\a (e, n) -> PQ.insert (k + vf e) (n, Just e) a) nq es
        apa = aux pa
        npa = if ncs
            then pa
            else apa
        nps = if ncs then ps ++ [(k, constructPath apa v)] else ps
        aux tpa = MB.maybe (M.insert vi Nothing tpa) (\e -> M.insert vi (Just (unpackStateValue getStartNodeN g e, e)) tpa) me


dijkstraGN :: (Ord a, Num a) => Int -> Int -> Direction
                             -> (Node -> Bool) -> (Edge -> Bool) -> (Label -> Bool) -> (Edge -> a)
                             -> Node -> GS [(a, Path)]
dijkstraGN de count di nbf ebf lbf vf n = do g <- get
                                             let ni = nodeId n
                                             return $ dijkstraG' g de count di nbf ebf lbf vf (PQ.singleton 0 (n, Nothing)) PQ.empty M.empty []


dijkstraG :: (Ord a, Num a) => Int -> Int -> Direction
                            -> (Node -> Bool) -> (Edge -> Bool) -> (Label -> Bool) -> (Edge -> a)
                            -> Id -> GS [(a, Path)]
dijkstraG de count di nbf ebf lbf vf i = getNodeByIdUnsafe i >>= dijkstraGN de count di nbf ebf lbf vf


dijkstraGTreeN :: (Ord a, Num a) => Int -> Int -> Direction
                                 -> (Node -> Bool) -> (Edge -> Bool) -> (Label -> Bool) -> (Edge -> a)
                                 -> Node -> GS PathTree
dijkstraGTreeN de count di nbf ebf lbf vf n = constructPathTreeW n <$> dijkstraGN de count di nbf ebf lbf vf n


dijkstraGTree :: (Ord a, Num a) => Int -> Int -> Direction
                                -> (Node -> Bool) -> (Edge -> Bool) -> (Label -> Bool) -> (Edge -> a)
                                -> Id -> GS PathTree
dijkstraGTree de count di nbf ebf lbf vf i = getNodeByIdUnsafe i >>= dijkstraGTreeN de count di nbf ebf lbf vf


dijkstraN :: (Ord a, Num a) => Int -> Int -> Direction
                            -> (Node -> Bool) -> [Label] -> (Edge -> a)
                            -> Node -> GS [(a, Path)]
dijkstraN de count di nbf ls = dijkstraGN de count di nbf (const True) (`elem` ls)


dijkstra :: (Ord a, Num a) => Int -> Int -> Direction
                           -> (Node -> Bool) -> [Label] -> (Edge -> a)
                           -> Id -> GS [(a, Path)]
dijkstra de count di nbf ls vf i = getNodeByIdUnsafe i >>= dijkstraN de count di nbf ls vf


dijkstraTreeN :: (Ord a, Num a) => Int -> Int -> Direction
                                -> (Node -> Bool) -> [Label] -> (Edge -> a)
                                -> Node -> GS PathTree
dijkstraTreeN de count di nbf ls vf n = constructPathTreeW n <$> dijkstraN de count di nbf ls vf n


dijkstraTree :: (Ord a, Num a) => Int -> Int -> Direction
                               -> (Node -> Bool) -> [Label] -> (Edge -> a)
                               -> Id -> GS PathTree
dijkstraTree de count di nbf ls vf i = getNodeByIdUnsafe i >>= dijkstraTreeN de count di nbf ls vf


dijkstraFieldN :: (Ord a, Num a) => Int -> Int -> Direction
                                 -> (Node -> Bool) -> [Label] -> Key -> a -> (Value -> a)
                                 -> Node -> GS [(a, Path)]
dijkstraFieldN de count di nbf ls k dt vvf = dijkstraGN de count di nbf (const True) (`elem` ls) (MB.maybe dt vvf . getEdgePropertySE k)


dijkstraField :: (Ord a, Num a) => Int -> Int -> Direction
                                -> (Node -> Bool) -> [Label] -> Key -> a -> (Value -> a)
                                -> Id -> GS [(a, Path)]
dijkstraField de count di nbf ls k dt vvf i = getNodeByIdUnsafe i >>= dijkstraFieldN de count di nbf ls k dt vvf


dijkstraFieldTreeN :: (Ord a, Num a) => Int -> Int -> Direction
                                     -> (Node -> Bool) -> [Label] -> Key -> a -> (Value -> a)
                                     -> Node -> GS PathTree
dijkstraFieldTreeN de count di nbf ls k dt vvf n = constructPathTreeW n <$> dijkstraFieldN de count di nbf ls k dt vvf n


dijkstraFieldTree :: (Ord a, Num a) => Int -> Int -> Direction
                                    -> (Node -> Bool) -> [Label] -> Key -> a -> (Value -> a)
                                    -> Id -> GS PathTree
dijkstraFieldTree de count di nbf ls k dt vvf i = getNodeByIdUnsafe i >>= dijkstraFieldTreeN de count di nbf ls k dt vvf



shortestPathGN :: Int -> Int -> Direction
                  -> (Node -> Bool) -> (Edge -> Bool) -> (Label -> Bool)
                  -> Node -> GS [(Int, Path)]
shortestPathGN de count di nbf ebf lbf = dijkstraGN de count di nbf ebf lbf (const 1)


shortestPathG :: Int -> Int -> Direction
                 -> (Node -> Bool) -> (Edge -> Bool) -> (Label -> Bool)
                 -> Id -> GS [(Int, Path)]
shortestPathG de count di nbf ebf lbf i = getNodeByIdUnsafe i >>= shortestPathGN de count di nbf ebf lbf


shortestPathGTreeN :: Int -> Int -> Direction
                      -> (Node -> Bool) -> (Edge -> Bool) -> (Label -> Bool)
                      -> Node -> GS PathTree
shortestPathGTreeN de count di nbf ebf lbf = dijkstraGTreeN de count di nbf ebf lbf (const (1 :: Int))


shortestPathGTree :: Int -> Int -> Direction
                     -> (Node -> Bool) -> (Edge -> Bool) -> (Label -> Bool)
                     -> Id -> GS PathTree
shortestPathGTree de count di nbf ebf lbf i = getNodeByIdUnsafe i >>= shortestPathGTreeN de count di nbf ebf lbf


shortestPathN :: Int -> Int -> Direction
                 -> (Node -> Bool) -> [Label]
                 -> Node -> GS [(Int, Path)]
shortestPathN de count di nbf ls = dijkstraN de count di nbf ls (const 1)


shortestPath :: Int -> Int -> Direction
                -> (Node -> Bool) -> [Label]
                -> Id -> GS [(Int, Path)]
shortestPath de count di nbf ls i = getNodeByIdUnsafe i >>= shortestPathN de count di nbf ls


shortestPathTreeN :: Int -> Int -> Direction
                     -> (Node -> Bool) -> [Label]
                     -> Node -> GS PathTree
shortestPathTreeN de count di nbf ls = dijkstraTreeN de count di nbf ls (const (1 :: Int))


shortestPathTree :: Int -> Int -> Direction
                    -> (Node -> Bool) -> [Label]
                    -> Id -> GS PathTree
shortestPathTree de count di nbf ls i = getNodeByIdUnsafe i >>= shortestPathTreeN de count di nbf ls
