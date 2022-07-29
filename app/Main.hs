
module Main where

import HittingSetModule (findLexicographicallyMinimalBlockingSetOfMinimumCardinality)

import qualified Data.IntSet                       as ISt
import qualified Data.Set                          as St

main :: IO ()
main = 
    putStrLn ("\n" ++ "findLexicographicallyMinimalBlockingSetOfMinimumCardinality ( St.fromList [ ISt.fromList [-3,1], ISt.fromList [6,1], ISt.fromList [3,-2,0] ] )    returns   "  ++ show (findLexicographicallyMinimalBlockingSetOfMinimumCardinality ( St.fromList [ ISt.fromList [-3,1], ISt.fromList [6,1], ISt.fromList [3,-2,0] ] )) ++ "\n")
