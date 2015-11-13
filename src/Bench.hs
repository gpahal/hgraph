module Main where

import           Criterion      (Benchmark, bench, nf)
import           Criterion.Main (bgroup, defaultMain)
import           HGraph.Path
import           HGraph.Types
import           SocialNetwork

benchmarks :: Int -> Int -> Int -> Int -> [Benchmark]
benchmarks u p f l =
    [ bench "creation"  (nf (const $ labelMaps $ execGraph (createRandomGraph u p f l) initialGraph) ())
    , bench "traversal" (nf (const $ length $ evalGraph (shortestPath 3 u DOUT ((>(u+p+1)) . fromInt . nodeId) [friendLabel] 1) $ execGraph (createRandomGraph u p f l) initialGraph) ())
    ]

main :: IO ()
main = defaultMain
    [ bgroup "50" (benchmarks 50 30 3 2)
    , bgroup "500" (benchmarks 500 400 8 4)
    , bgroup "2000" (benchmarks 2000 1500 12 5)
    ]
