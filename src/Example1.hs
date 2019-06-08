module Example1 where

-- Example 18 from arXiv 1807.11675

import qualified WeightedLPA as WLPA
import Graph
import qualified Data.Map as M
import qualified Data.Set as S

weighted_graph_G :: WeightedGraph String String
weighted_graph_G = WeightedGraph (buildGraphFromEdges [("e",("v","u")),("f",("v","u"))]) (M.fromList [("e",1),("f",2)])

unweighted_graph_H :: Graph String String
unweighted_graph_H = Graph (S.fromList ["u1","u2","u3","v"]) (M.fromList [("j",("u3","u3")),("i",("u3","u1")),("h",("u3","u2")),("g",("u2","u1")),("e1",("v","u1")),("e2",("v","u2")),("e3",("v","u3")),("f",("v","u2"))])

-- this checks the isomorphism L_K(E,w) ~= L_K(F)
testIsomorphism = WLPA.wLPA_relations_present phi weighted_graph_G (convertGraphToWeighted unweighted_graph_H)

phi = phi'
 where
  phi' (WLPA.AEdge "e" 1) = e1 + e2 + e3
  phi' (WLPA.AEdge "f" 1) = f
  phi' (WLPA.AEdge "f" 2) = f * g + e1 * s i + e2 * s h + e3 * s j
  phi' (WLPA.AVertex "u") = u1 + u2 + u3
  phi' (WLPA.AVertex "v") = v
  phi' (WLPA.AGhostEdge e w) = WLPA.adjoint (phi' (WLPA.AEdge e w))
  phi' _ = WLPA.Zero

  v = vertex "v"
  u1 = vertex "u1"
  u2 = vertex "u2"
  u3 = vertex "u3"

  e1 = edge "e1"
  e2 = edge "e2"
  e3 = edge "e3"
  f = edge "f"
  g = edge "g"
  i = edge "i"
  j = edge "j"
  h = edge "h"

-- this checks the isomorphism L_K(E,w) ~= L_K(F)
testInverseIsomorphism = WLPA.wLPA_relations_present psi (convertGraphToWeighted unweighted_graph_H) weighted_graph_G

psi = psi'
 where
  psi' (WLPA.AVertex "v") = v_
  psi' (WLPA.AVertex "u1") = u1_
  psi' (WLPA.AVertex "u2") = u2_
  psi' (WLPA.AVertex "u3") = u3_
  psi' (WLPA.AEdge "e1" 1) = e1_
  psi' (WLPA.AEdge "e2" 1) = e2_
  psi' (WLPA.AEdge "e3" 1) = e3_
  psi' (WLPA.AEdge "f" 1) = f_
  psi' (WLPA.AEdge "g" 1) = g_
  psi' (WLPA.AEdge "h" 1) = h_
  psi' (WLPA.AEdge "i" 1) = i_
  psi' (WLPA.AEdge "j" 1) = j_
  psi' (WLPA.AGhostEdge e w) = WLPA.adjoint (psi' (WLPA.AEdge e w))
  psi' _ = WLPA.Zero

  v_ = v
  u1_ = (s f2) * f1 * (s f1) * f2
  u2_ = (s f1) * f2 * (s f2) * f1
  u3_ = u - u1_ - u2_
  e1_ = e1 - e2_ - f2 * j_
  e2_ = e1 * u2_
  e3_ = f2 * j_
  f_ = f1
  g_ = (s f1) * f2
  h_ = (s f2) * e2_
  i_ = (s f2) * (e1 - e2_ - f2 * j_)
  j_ = let a = (s (e1 - e2_)) * f2 in s (u3_ * a)
  
  u = vertex "u"
  v = vertex "v"
  e1 = edge "e"
  f1 = edge "f"
  f2 = edge2 "f"

-- this tests that the composition map is the identity on the generators
testComposition = do
  putStr $ unlines $ map show $ zip tests (map atom generators)
  putStrLn $ show $ and tests
 where
  tests = zipWith (WLPA.equal_wrt_graph weighted_graph_G) (map atom generators) (map composition generators)
  generators = [WLPA.AEdge "e" 1,WLPA.AEdge "f" 1,WLPA.AEdge "f" 2,WLPA.AVertex "u",WLPA.AVertex "v"]

composition = WLPA.gmap psi . phi

atom = WLPA.Atom 1
vertex = atom . WLPA.vertex
edge = atom . (flip WLPA.edge 1)
ghostEdge = atom . (flip WLPA.ghostEdge 1)

edge2 = atom . (flip WLPA.edge 2)
ghostEdge2 = atom . (flip WLPA.ghostEdge 2)

s = WLPA.adjoint
