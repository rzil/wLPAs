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

weighted_graph_G' :: WeightedGraph String String
weighted_graph_G' = WeightedGraph (buildGraphFromEdges [("e",("v","u")),("f",("v","u")),("g",("v","u")),("h",("v","u"))]) (M.fromList [("e",1),("f",2),("g",1),("h",2)])

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
phi (WLPA.AVertex "v") = setElem v (2,2) $ setElem v (1,1) (zero 3 3)
phi (WLPA.AVertex "u") = setElem v (3,3) (zero 3 3)
phi (WLPA.AEdge "e" 1) = setElem e1 (1,3) (zero 3 3)
phi (WLPA.AEdge "f" 1) = setElem f1 (1,3) (zero 3 3)
phi (WLPA.AEdge "f" 2) = setElem f2 (1,3) (zero 3 3)
phi (WLPA.AEdge "g" 1) = setElem e1 (2,3) (zero 3 3)
phi (WLPA.AEdge "h" 1) = setElem f1 (2,3) (zero 3 3)
phi (WLPA.AEdge "h" 2) = setElem f2 (2,3) (zero 3 3)
phi (WLPA.AGhostEdge e w) = adjoint (phi (WLPA.AEdge e w))
phi _ = zero 3 3

testHomomorphism :: [Matrix (WLPA.Term String String Integer)]
testHomomorphism = WLPA.wLPA_relations_map' (zero 3 3) phi weighted_graph_G'

testHomomorphism' :: [Matrix (WLPA.Term String String Integer)]
testHomomorphism' = WLPA.wLPA_relations_map' (zero 9 9) rho weighted_graph_G'

rho :: WLPA.AtomType String String -> Matrix (WLPA.Term String String Integer)
rho = rho'
 where
  rho' (WLPA.AVertex "v") = concatMat $ setElem v' (2,2) $ setElem v' (1,1) z
  rho' (WLPA.AVertex "u") = concatMat $ setElem v' (3,3) z
  rho' (WLPA.AEdge "e" 1) = concatMat $ setElem e1' (1,3) z
  rho' (WLPA.AEdge "f" 1) = concatMat $ setElem f1' (1,3) z
  rho' (WLPA.AEdge "f" 2) = concatMat $ setElem f2' (1,3) z
  rho' (WLPA.AEdge "g" 1) = concatMat $ setElem e1' (2,3) z
  rho' (WLPA.AEdge "h" 1) = concatMat $ setElem f1' (2,3) z
  rho' (WLPA.AEdge "h" 2) = concatMat $ setElem f2' (2,3) z
  rho' (WLPA.AGhostEdge e w) = adjoint (rho' (WLPA.AEdge e w))
  rho' _ = zero 9 9
  
  z = matrix 3 3 (const (zero 3 3))
  v' = c (vertex "u" + vertex "v")
  e1' = x (e1 + s f2)
  f1' = x (f1 + s f1)
  f2' = x (f2 + s e1)
  c y = setElem y (1,1) $ setElem y (2,2) $ setElem y (3,3) (zero 3 3)
  x y = setElem y (1,3) $ setElem y (2,1) $ setElem y (3,2) (zero 3 3)

concatMat :: Matrix (Matrix a) -> Matrix a
concatMat m = matrix 9 9 (\(i,j) -> let (r,c)=(i-1,j-1) in (m ! (r`div`3 + 1,c`div`3 + 1)) ! (r`mod`3 + 1,c`mod`3 + 1))

fmapBasisForm wgraph = fmap (WLPA.convertToBasisForm wgraph)

check' = map (fmapBasisForm weighted_graph_G) testHomomorphism'

check = map (fmapBasisForm weighted_graph_E) testHomomorphism

showTest' = do
   putStr $ unlines $ map show (zip checks rels)
   putStrLn (show $ and checks)
 where
  checks = map (matrix 9 9 (const []) ==) check'
  rels = WLPA.wLPA_relations_show weighted_graph_G'

showTest = do
   putStr $ unlines $ map show (zip checks rels)
   putStrLn (show $ and checks)
 where
  checks = map (matrix 3 3 (const []) ==) check
  rels = WLPA.wLPA_relations_show weighted_graph_G'
