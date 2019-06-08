module Example9 where

import qualified WeightedLPA as WLPA
import Graph
import qualified Data.Map as M
import qualified Data.Set as S

weighted_graph_E :: WeightedGraph String String
weighted_graph_E = WeightedGraph (buildGraphFromEdges [("e",("v","v")),("f",("v","v"))]) (M.fromList [("e",1),("f",2)])

unweighted_graph_J :: Graph String String
unweighted_graph_J = buildGraphFromEdges [ ("g",("u2","u1")), ("h",("u3","u2")), ("i",("u3","u1")), ("j",("u3","u3")) ]

toeplitz_graph :: Graph String String
toeplitz_graph = buildGraphFromEdges [("a",("u2","u2")),("b",("u2","u1"))]

-- this tests we have a well defined homomorphism
testHomomorphism = WLPA.wLPA_relations_present phi (convertGraphToWeighted unweighted_graph_J) weighted_graph_E
 where
  phi (WLPA.AVertex "u1") = u1
  phi (WLPA.AVertex "u2") = u2
  phi (WLPA.AVertex "u3") = u3
  phi (WLPA.AEdge "g" 1) = g
  phi (WLPA.AEdge "h" 1) = h
  phi (WLPA.AEdge "i" 1) = i
  phi (WLPA.AEdge "j" 1) = j
  phi (WLPA.AGhostEdge e w) = WLPA.adjoint (phi (WLPA.AEdge e w))
  phi _ = WLPA.Zero 
  
  v = vertex "v"
  e1 = edge "e"
  f1 = edge "f"
  f2 = edge2 "f"
  
  x = s f1 * f2
  y = s f2 * e1
  z = s f2 * f2
  
  u1 = s x * x
  u2 = x * s x
  u3 = s y * y - s x * x - x * s x
  g = x
  h = y - y * z
  i = y * (z - y * s y)
  j = y^2 * s y

-- this tests for a homomorphism from the Toeplitz algebra
testToeplitzHomomorphism = WLPA.wLPA_relations_present phi (convertGraphToWeighted toeplitz_graph) (convertGraphToWeighted unweighted_graph_J)
 where
  phi (WLPA.AVertex "u1") = vertex "u1" + vertex "u2"
  phi (WLPA.AVertex "u2") = vertex "u3"
  phi (WLPA.AEdge "a" 1) = edge "j"
  phi (WLPA.AEdge "b" 1) = edge "h" + edge "i"
  phi (WLPA.AGhostEdge e w) = WLPA.adjoint (phi (WLPA.AEdge e w))
  phi _ = WLPA.Zero 

atom c = WLPA.Atom c
vertex = atom 1 . WLPA.vertex
edge = atom 1 . (flip WLPA.edge 1)
ghostEdge = atom 1 . (flip WLPA.ghostEdge 1)

edge2 = atom 1 . (flip WLPA.edge 2)
ghostEdge2 = atom 1 . (flip WLPA.ghostEdge 2)

s = WLPA.adjoint
