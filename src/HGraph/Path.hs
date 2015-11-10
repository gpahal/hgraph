module HGraph.Path where

import           Control.Applicative
import           Control.Monad.State
import qualified Data.Map             as M
import qualified Data.Maybe           as MB
import qualified Data.PQueue.Prio.Min as PQ
import           HGraph.Edge
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
                             -> PQ.MinPQueue a Node -> PQ.MinPQueue a Node -> PathAncestors
                             -> [(a, Path)] -> [(a, Path)]
dijkstraG' g de count di nbf ebf lbf vf cq nq pa ps
    | de == 0 || count == 0 = ps
    | cqn && nqn        = ps
    | cqn               = dijkstraG' g (de - 1) count di nbf ebf lbf vf nq PQ.empty pa ps
    | otherwise         = dijkstraG' g de nn di nbf ebf lbf vf ncq nnq npa nps
    where
        cqn           = PQ.null cq
        nqn           = PQ.null nq
        ((k, v), ncq) = PQ.deleteFindMin cq
        ncs           = nbf v
        nn  = if ncs then count - 1 else count
        gsEdges
            | di == DOUT  = getFilteredOutNodes ebf lbf v
            | di == DIN   = getFilteredInNodes ebf lbf v
            | di == DBOTH = combineGS (++) (getFilteredInNodes ebf lbf v) (getFilteredOutNodes ebf lbf v)
            | otherwise   = return []
        es = evalState gsEdges g
        nnq = if ncs then nq else foldl (\a b -> PQ.insert (k + vf (fst b)) (snd b) a) nq es
        npa = if ncs then pa else foldl (\a b -> M.insertWith (\_ y -> y) (nodeId $ snd b) (Just (v, fst b)) a) pa es
        nps = if ncs then ps ++ [(k, constructPath pa v)] else ps

dijkstraG :: (Ord a, Num a) => Int -> Int -> Direction
                            -> (Node -> Bool) -> (Edge -> Bool) -> (Label -> Bool) -> (Edge -> a)
                            -> Node -> GS [(a, Path)]
dijkstraG de count di nbf ebf lbf vf n = do g <- get
                                            return $ dijkstraG' g de count di nbf ebf lbf vf (PQ.singleton 0 n) PQ.empty M.empty []

dijkstraGTree :: (Ord a, Num a) => Int -> Int -> Direction
                                -> (Node -> Bool) -> (Edge -> Bool) -> (Label -> Bool) -> (Edge -> a)
                                -> Node -> GS PathTree
dijkstraGTree de count di nbf ebf lbf vf n = constructPathTreeW n <$> dijkstraG de count di nbf ebf lbf vf n

dijkstra :: (Ord a, Num a) => Int -> Int -> Direction
                           -> (Node -> Bool) -> [Label] -> (Edge -> a)
                           -> Node -> GS [(a, Path)]
dijkstra de count di nbf ls = dijkstraG de count di nbf (const True) (`elem` ls)

dijkstraTree :: (Ord a, Num a) => Int -> Int -> Direction
                               -> (Node -> Bool) -> [Label] -> (Edge -> a)
                               -> Node -> GS PathTree
dijkstraTree de count di nbf ls vf n = constructPathTreeW n <$> dijkstra de count di nbf ls vf n

dijkstraField :: (Ord a, Num a) => Int -> Int -> Direction
                                -> (Node -> Bool) -> [Label] -> Key -> a -> (Value -> a)
                                -> Node -> GS [(a, Path)]
dijkstraField de count di nbf ls k dt vvf = dijkstraG de count di nbf (const True) (`elem` ls) (MB.maybe dt vvf . getEdgePropertySE k)

dijkstraFieldTree :: (Ord a, Num a) => Int -> Int -> Direction
                                    -> (Node -> Bool) -> [Label] -> Key -> a -> (Value -> a)
                                    -> Node -> GS PathTree
dijkstraFieldTree de count di nbf ls k dt vvf n = constructPathTreeW n <$> dijkstraField de count di nbf ls k dt vvf n

shortestPathG :: Int -> Int -> Direction
                 -> (Node -> Bool) -> (Edge -> Bool) -> (Label -> Bool)
                 -> Node -> GS [(Int, Path)]
shortestPathG de count di nbf ebf lbf = dijkstraG de count di nbf ebf lbf (const 1)

shortestPathGTree :: Int -> Int -> Direction
                     -> (Node -> Bool) -> (Edge -> Bool) -> (Label -> Bool)
                     -> Node -> GS PathTree
shortestPathGTree de count di nbf ebf lbf = dijkstraGTree de count di nbf ebf lbf (const (1 :: Int))

shortestPath :: Int -> Int -> Direction
                -> (Node -> Bool) -> [Label]
                -> Node -> GS [(Int, Path)]
shortestPath de count di nbf ls = dijkstra de count di nbf ls (const 1)

shortestPathTree :: Int -> Int -> Direction
                    -> (Node -> Bool) -> [Label]
                    -> Node -> GS PathTree
shortestPathTree de count di nbf ls = dijkstraTree de count di nbf ls (const (1 :: Int))
