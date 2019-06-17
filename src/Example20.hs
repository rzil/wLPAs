module Example20 where

import qualified WeightedLPA as WLPA
import Graph
import GraphVisualise
import qualified Data.Map as M
import qualified Data.Set as S

import Data.Matrix

weighted_graph_E :: WeightedGraph String String
weighted_graph_E = WeightedGraph (buildGraphFromEdges [("e",("v","v")),("f",("v","v"))]) (M.fromList [("e",1),("f",2)])

weighted_graph_G :: WeightedGraph String String
weighted_graph_G = WeightedGraph (buildGraphFromEdges [("e",("v","u")),("f",("v","u"))]) (M.fromList [("e",1),("f",2)])

weighted_graph_G3 :: WeightedGraph String String
weighted_graph_G3 = WeightedGraph (buildGraphFromEdges [("e",("v","u")),("f",("v","u")),("g",("u","t")),("h",("u","t"))]) (M.fromList [("e",1),("f",2),("g",1),("h",2)])

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
phi (WLPA.AVertex "v") = setElem v (1,1) (zero 3 3)
phi (WLPA.AVertex "u") = setElem v (2,2) (zero 3 3)
phi (WLPA.AVertex "t") = setElem v (3,3) (zero 3 3)
phi (WLPA.AEdge "e" 1) = setElem e1 (1,2) (zero 3 3)
phi (WLPA.AEdge "f" 1) = setElem f1 (1,2) (zero 3 3)
phi (WLPA.AEdge "f" 2) = setElem f2 (1,2) (zero 3 3)
phi (WLPA.AEdge "g" 1) = setElem e1 (2,3) (zero 3 3)
phi (WLPA.AEdge "h" 1) = setElem f1 (2,3) (zero 3 3)
phi (WLPA.AEdge "h" 2) = setElem f2 (2,3) (zero 3 3)
phi (WLPA.AGhostEdge e w) = adjoint (phi (WLPA.AEdge e w))
phi _ = zero 3 3

testHomomorphism :: [Matrix (WLPA.Term String String Integer)]
testHomomorphism = WLPA.wLPA_relations_map' (zero 3 3) phi weighted_graph_G3

testHomomorphism' :: [Matrix (WLPA.Term String String Integer)]
testHomomorphism' = WLPA.wLPA_relations_map' (zero 15 15) rho weighted_graph_G3

rho :: WLPA.AtomType String String -> Matrix (WLPA.Term String String Integer)
rho = rho'
 where
  rho' (WLPA.AVertex "v") = concatMat $  setElem v' (1,1) z
  rho' (WLPA.AVertex "u") = concatMat $  setElem v' (2,2) z
  rho' (WLPA.AVertex "t") = concatMat $ setElem v' (3,3) z
  rho' (WLPA.AEdge "e" 1) = concatMat $ setElem e1' (1,2) z
  rho' (WLPA.AEdge "f" 1) = concatMat $ setElem f1' (1,2) z
  rho' (WLPA.AEdge "f" 2) = concatMat $ setElem f2' (1,2) z
  rho' (WLPA.AEdge "g" 1) = concatMat $ setElem e1' (2,3) z
  rho' (WLPA.AEdge "h" 1) = concatMat $ setElem f1' (2,3) z
  rho' (WLPA.AEdge "h" 2) = concatMat $ setElem f2' (2,3) z
  rho' (WLPA.AGhostEdge e w) = adjoint (rho' (WLPA.AEdge e w))
  rho' _ = zero 15 15
  
  z = matrix 3 3 (const (zero 5 5))
  v' = c (vertex "u" + vertex "v")
  e1' = x (e1 + s f2)
  f1' = x (f1 + s f1)
  f2' = x (f2 + s e1)
  c y = setElem y (1,1) $ setElem y (2,2) $ setElem y (3,3) $ setElem y (4,4) $ setElem y (5,5) (zero 5 5)
  x y = setElem y (1,5) $ setElem y (2,1) $ setElem y (3,2) $ setElem y (4,3) $ setElem y (5,4) (zero 5 5)

concatMat :: Matrix (Matrix a) -> Matrix a
concatMat m = matrix 15 15 (\(i,j) -> let (r,c)=(i-1,j-1) in (m ! (r`div`5 + 1,c`div`5 + 1)) ! (r`mod`5 + 1,c`mod`5 + 1))

fmapBasisForm wgraph = fmap (WLPA.convertToBasisForm wgraph)

check' = map (fmapBasisForm weighted_graph_G) testHomomorphism'

check = map (fmapBasisForm weighted_graph_E) testHomomorphism

showTest' = do
   putStr $ unlines $ map show (zip checks rels)
   putStrLn (show $ and checks)
 where
  checks = map (matrix 15 15 (const []) ==) check'
  rels = WLPA.wLPA_relations_show weighted_graph_G3

showTest = do
   putStr $ unlines $ map show (zip checks rels)
   putStrLn (show $ and checks)
 where
  checks = map (matrix 3 3 (const []) ==) check
  rels = WLPA.wLPA_relations_show weighted_graph_G3
