module Example19 where

import qualified WeightedLPA as WLPA
import Graph
import GraphVisualise
import qualified Data.Map as M
import qualified Data.Set as S

import Data.Matrix

weighted_graph_E :: WeightedGraph String String
weighted_graph_E = WeightedGraph (buildGraphFromEdges [("e",("v","v")),("f",("v","v"))]) (M.fromList [("e",1),("f",2)])

weighted_graph_G :: WeightedGraph String String
weighted_graph_G = WeightedGraph (buildGraphFromEdges [("e",("v","u")),("f",("v","u")),("g",("v","w")),("h",("v","w"))]) (M.fromList [("e",1),("f",2),("g",1),("h",2)])

atom = WLPA.Atom 1
vertex = atom . WLPA.vertex
edge = atom . (flip WLPA.edge 1)
ghostEdge = atom . (flip WLPA.ghostEdge 1)

edge2 = atom . (flip WLPA.edge 2)
ghostEdge2 = atom . (flip WLPA.ghostEdge 2)

s = WLPA.adjoint

v = vertex "v"

e1 = edge "e"
f1 = edge "f"
f2 = edge2 "f"

adjoint m = fmap s (transpose m)

phi :: WLPA.AtomType String String -> Matrix (WLPA.Term String String Integer)
phi (WLPA.AVertex "v") = setElem v (3,3) $ setElem v (1,1) (zero 4 4)
phi (WLPA.AVertex "u") = setElem v (2,2) (zero 4 4)
phi (WLPA.AVertex "w") = setElem v (4,4) (zero 4 4)
phi (WLPA.AEdge "e" 1) = setElem e1 (1,2) (zero 4 4)
phi (WLPA.AEdge "f" 1) = setElem f1 (1,2) (zero 4 4)
phi (WLPA.AEdge "f" 2) = setElem f2 (1,2) (zero 4 4)
phi (WLPA.AEdge "g" 1) = setElem e1 (3,4) (zero 4 4)
phi (WLPA.AEdge "h" 1) = setElem f1 (3,4) (zero 4 4)
phi (WLPA.AEdge "h" 2) = setElem f2 (3,4) (zero 4 4)
phi (WLPA.AGhostEdge e w) = adjoint (phi (WLPA.AEdge e w))
phi _ = zero 4 4

testHomomorphism :: [Matrix (WLPA.Term String String Integer)]
testHomomorphism = WLPA.wLPA_relations_map' (zero 4 4) phi weighted_graph_G

fmapBasisForm wgraph = fmap (WLPA.convertToBasisForm wgraph)

check = map (fmapBasisForm weighted_graph_E) testHomomorphism

showTest = do
   putStr $ unlines $ map show (zip checks rels)
   putStrLn (show $ and checks)
 where
  checks = map (matrix 4 4 (const []) ==) check
  rels = WLPA.wLPA_relations_show weighted_graph_G
