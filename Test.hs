module Test where

import Inf2d1
import System.Environment

main :: IO ()
main = do
    [s, g] <- getArgs
    let start = read s
        dest  = read g
    print $ breadthFirstSearch dest next [[start]] []
    print $ depthFirstSearch dest next [[start]] []
    print $ iterDeepSearch dest next start 1
    print $ bestFirstSearch dest next (manhattan dest) [[start]] []
    print $ aStarSearch dest next (manhattan dest) cost [[start]] []
