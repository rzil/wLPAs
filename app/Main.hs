module Main where

import qualified Example1 as GH
import qualified Example9 as JE
import qualified Example11 as EG
import qualified Example15 as FGx (showTest)
import qualified Example18 as GnE (showTest)
import qualified Example20 as G3G (showTest)
import qualified Example21 as HG (showTest)

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
    printSubheading "Example 5.0.3 from my thesis"
    runTest "L_K(E,w) --> L_K(F)" GH.testIsomorphism
    runTest "L_K(F) --> L_K(E,w)" GH.testInverseIsomorphism
    runTest "L_K(E,w) ~= L_K(F)" GH.testComposition
    
    printSubheading "Example 5.2.1 from my thesis"
    runTest "L_K(F) --> L_K(E)" JE.testHomomorphism
    
    printSubheading "Example 5.2.2 from my thesis"
    runTest "L_K(Toeplitz) --> L_K(F)" JE.testToeplitzHomomorphism
    
    printSubheading "Example 5.2.3 from my thesis"
    runTest "η" EG.testOriginalHomomorphism
    runTest "ρ" EG.testOriginalEpimorphism
    runTest "θ" EG.testOriginalHomomorphismComposition
    
    printSubheading "Example 5.2.4 from my thesis. Can test for any n."
    runTest "L_K(G_n,w_{G_n}) --> M_{L_K(E,w_E)}(n) for n = 5" (GnE.showTest 5)
    
    printSubheading "Example 5.2.5 from my thesis"
    runTest "L_K(E,w_E) --> L_{K[x,x^{-1}]}(G,w_G)" FGx.showTest
    
    printSubheading "Example 5.2.6 from my thesis"
    runTest "L_K(G_n,w_{G_n}) --> M_{L_K(G,w_G)}(2n^2 - n) for n = 3" (G3G.showTest)
    
    printSubheading "Example 5.2.7 from my thesis"
    runTest "L_K(H,w_H) --> M_{L_K(G,w_G)}(3)" HG.showTest
