module Main where

import qualified Example1 as GH
import qualified Example9 as JE
import qualified Example11 as EG
import qualified Example15 as FGx (showTest)
import qualified Example18 as GnE (showTest)

runTest name test = do
    putStrLn "===="
    putStr "Testing "
    putStrLn name
    test
    putStrLn "===="

printSubheading str = do
   putStrLn "===="
   putStrLn str
   putStrLn "===="

main :: IO ()
main = do
    printSubheading "Example 4 from my thesis"
    runTest "L_K(E,w) --> L_K(F)" GH.testIsomorphism
    runTest "L_K(F) --> L_K(E,w)" GH.testInverseIsomorphism
    runTest "L_K(E,w) ~= L_K(F)" GH.testComposition
    
    printSubheading "Example 5 from my thesis"
    runTest "L_K(F) --> L_K(E)" JE.testHomomorphism
    
    printSubheading "Example 6 from my thesis"
    runTest "L_K(Toeplitz) --> L_K(F)" JE.testToeplitzHomomorphism
    
    printSubheading "Example 7 from my thesis"
    runTest "ξ" EG.testOriginalHomomorphism
    runTest "ρ" EG.testOriginalEpimorphism
    runTest "θ" EG.testOriginalHomomorphismComposition
    
    printSubheading "Example 8 from my thesis. Can test for any n."
    runTest "L_K(G_n,w_{G_n}) --> M_R(n) for n = 5" (GnE.showTest 5)
    
    printSubheading "Example 9 from my thesis"
    runTest "L_K(E,w_E) --> L_{K[x,x^{-1}]}(G,w_G)" FGx.showTest
