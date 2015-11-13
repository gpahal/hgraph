module Main where

import           Criterion      (Benchmark, bench, nf)
import           Criterion.Main (bgroup, defaultMain)
import           HGraph.Path
import           HGraph.Types
import           SocialNetwork

benchmarks :: Int -> Int -> Int -> Int -> [Benchmark]
benchmarks u p f l =
    let g = execGraph (createRandomGraph u p f l) initialGraph in
    [ bench "creation"  (nf (const $ labelMaps g) ())
    , bench "traversal" (nf (const $ length $ evalGraph (shortestPath 3 u DOUT ((>(u+p+1)) . fromInt . nodeId) [friendLabel] 1) g) ())
    ]

main :: IO ()
main = defaultMain
    [ bgroup "50" (benchmarks 50 30 3 2)
    , bgroup "100" (benchmarks 100 80 4 2)
    , bgroup "200" (benchmarks 200 150 6 3)
    ]
