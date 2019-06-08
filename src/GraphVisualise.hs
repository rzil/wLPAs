module GraphVisualise where

import Graph
import qualified Data.Map as M
import qualified Data.Set as S
import System.Process (callCommand)

-- creates a GraphViz .dot file string for the graph
dotString :: (Show a, Show e, Ord a, Ord e) => Graph e a -> String
dotString graph = "digraph {\n" ++ unlines ([let (u,v) = ((edges graph) M.! e) in " " ++ (show.show) u ++ " -> " ++ (show.show) v ++ " [ label=" ++ (show.(" " ++).(++ " ").show) e ++ " ]" | e <- M.keys (edges graph)] ++ [" " ++ (show.show) u | (u,[]) <- M.assocs (vertexForm graph)]) ++ "}\n"

visualiseGraph :: (Show a, Show e, Ord a, Ord e) => Graph e a -> IO ()
visualiseGraph graph = writeFile "test.dot" (dotString graph) >> callCommand "dot test.dot -Tpng > test.png; open test.png"

weightedDotString :: (Show a, Show e, Ord e, Ord a) => WeightedGraph e a -> String
weightedDotString (WeightedGraph graph weighting) = "digraph {\n" ++ unlines ([let (u,v) = ((edges graph) M.! e) in " " ++ (show.show) u ++ " -> " ++ (show.show) v ++ " [ label=" ++ (show.(" " ++).(++ " ").show) (e,(weighting M.! e)) ++ " ]" | e <- M.keys (edges graph)] ++ [" " ++ (show.show) u | (u,[]) <- M.assocs (vertexForm graph)]) ++ "}\n"

visualiseWeightedGraph :: (Ord a, Ord e, Show e, Show a) => WeightedGraph e a -> IO ()
visualiseWeightedGraph weightedGraph = writeFile "test.dot" (weightedDotString weightedGraph) >> callCommand "dot test.dot -Tpng > test.png; open test.png"
