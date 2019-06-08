module GraphRandom where

import Graph
import System.Random
import qualified Data.Map as M
import qualified Data.Set as S

randomGraph :: Int -> Int -> Float -> Graph String Int
randomGraph seed n p = Graph (S.fromList [1..n]) (M.fromList $ zip ["e"++show k | k <- [1..]] [v | (k,v) <- zip (randomRs (0.0,1.0) (mkStdGen seed)) [(x,y) | x <- [1..n], y <- [1..n], x /= y], k < p])

randomHamiltonianGraph :: Int -> Int -> Float -> Graph String Int
randomHamiltonianGraph seed n p = graphUnion (cycleGraph n) (randomGraph seed n p)
