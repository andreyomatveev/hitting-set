-- Andrey O. Matveev
-- In the spirit of Theorem 9.12(ii) and Example 9.13 from the monograph A.O. Matveev, Symmetric Cycles,
-- Jenny Stanford Publishing, 2023, https://www.jennystanford.com/ .
--
-- Let AA := {A_1, A_2, ..., A_k} be a nonempty family of nonempty subsets of a finite set of integers E.
-- A subset B of the set E is called a blocking set [or hitting set, transversal, vertex cover (or node cover),
-- system of representatives] of the family AA if and only if the set B has a nonempty
-- intersection with each set A_i from the family AA.
--
-- The findLexicographicallyMinimalBlockingSetOfMinimumCardinality function takes a moderate-size family of moderate-size sets of integers, and
-- it slowly returns the corresponding
--                                      lexicographically minimal blocking set
--                                          of minimum cardinality.
--
-- Integer-Linear-Programming-free.
--
-- Working with subsets of a finite set and with their orderings, we use the techniques presented in Section 2.3
-- of the classical monograph D.L. Kreher, D.R. Stinson, Combinatorial Algorithms: Generation,
-- Enumeration and Search, CRC Press series on discrete mathematics and its applications. Boca Raton, FL: CRC Press, 1999.
--
-- On the-state-of-the art, and on advanced topics, see, e.g., the works:
--
-- van Bevern R., Smirnov P.V. Optimal-size problem kernels for d-Hitting Set in linear time and space. Information
-- Processing Letters, 2020, 163, and
--
-- Kruchinin, V., Shablya, Y., Kruchinin, D., Rulevskiy V., Unranking small combinations of a large Set
-- in co-lexicographic order. Algorithms, 2022, 15, 36.

module HittingSetModule
  ( findLexicographicallyMinimalBlockingSetOfMinimumCardinality
  ) where

import           Data.Bit                          (Bit (..))
import qualified Data.IntMap.Strict                as DIMS
import qualified Data.IntSet                       as ISt
import           Data.List                         (elemIndex, length)
import           Data.Maybe                        (fromMaybe, isNothing)
import qualified Data.Set                          as St
import           Math.Combinatorics.Exact.Binomial (choose)

findLexicographicallyMinimalBlockingSetOfMinimumCardinality ::
     St.Set ISt.IntSet -> ISt.IntSet
-- Call for instance
--    ghci> findLexicographicallyMinimalBlockingSetOfMinimumCardinality ( St.fromList [ ISt.fromList [-3,1], ISt.fromList [6,1], ISt.fromList [3,-2,0] ] )
-- to get the result:
--    fromList [-2,1]
--
-- Call
--    ghci> :{
--          findLexicographicallyMinimalBlockingSetOfMinimumCardinality
--          (St.fromList
--          [
--          ISt.fromList [-1,7,2,4,6,0,11,-12,15],
--          ISt.fromList [2,3,5,9,12,-14,17,-25,56],
--          ISt.fromList [33,46,-10],
--          ISt.fromList [5,8,14,4,6,11,17,15]
--          ]
--          )
--          :}
-- to get the sesult:
--    fromList [-25,-10,4]
findLexicographicallyMinimalBlockingSetOfMinimumCardinality family
  | family == St.empty = ISt.singleton (-11111) -- "N/A: The family should be nonempty"
  | St.size (St.filter (\s -> ISt.size s == 0) family) > 0 =
    ISt.singleton (-22222) -- "N/A: The family should not contain empty sets"
  | St.size family == 1 = ISt.singleton (ISt.findMin (St.elemAt 0 family))
  | otherwise = do
    let fst = St.elemAt 0 family
    let itt = inters (St.delete fst family) fst
    if ISt.size itt > 0
      then ISt.singleton (ISt.findMin itt)
      else do
        let uni = ISt.unions (St.toList family)
        let vertices = ISt.toList uni
        let sizeOfVertexSet = Data.List.length vertices
        if sizeOfVertexSet == St.size family
          then uni
          else if disj (St.delete fst family) fst
                 then do
                   let bS = ISt.empty
                   St.foldr
                     ISt.union
                     bS
                     (St.map (ISt.singleton . ISt.findMin) family)
                 else do
                   let familyRenamed =
                         St.map
                           (ISt.map
                              (\e ->
                                 fromMaybe 0 (Data.List.elemIndex e vertices) +
                                 1))
                           family
                   let complementOfSolution =
                         unRankLex
                           sizeOfVertexSet
                           (findTheIndex sizeOfVertexSet familyRenamed 1)
                   let hSRenamed =
                         ISt.fromList
                           [ e
                           | e <- [1 .. sizeOfVertexSet]
                           , e `ISt.notMember` complementOfSolution
                           ]
                   ISt.map (\e -> vertices !! (e - 1)) hSRenamed

findTheIndex :: Int -> St.Set ISt.IntSet -> Integer -> Integer
findTheIndex sizeOfVertexSet family currentIndex = do
  let componentsAlreadyCalculated = DIMS.empty
  if snd
       (componentFunctionOfFamily
          sizeOfVertexSet
          family
          currentIndex
          componentsAlreadyCalculated
          False)
    then currentIndex
    else findTheIndex sizeOfVertexSet family (currentIndex + 1)

componentFunctionOfFamily ::
     Int
  -> St.Set ISt.IntSet
  -> Integer
  -> DIMS.IntMap Bit
  -> Bool
  -> (DIMS.IntMap Bit, Bool)
componentFunctionOfFamily sizeOfVertexSet remainingSubfamily currentIndex componentsAlreadyCalculated returnFalse
  | returnFalse = (componentsAlreadyCalculated, False)
  | remainingSubfamily == St.empty = (componentsAlreadyCalculated, True)
  | otherwise = do
    let firstRemainingIntSet = St.elemAt 0 remainingSubfamily
    let componentPlus =
          componentFunctionOfIntSet
            sizeOfVertexSet
            firstRemainingIntSet
            currentIndex
            componentsAlreadyCalculated
            False
    if not (snd componentPlus)
      then componentFunctionOfFamily
             sizeOfVertexSet
             remainingSubfamily -- we do not make remainingSubfamily smaller because we are going to stop right now
             currentIndex
             componentsAlreadyCalculated
             True
      else componentFunctionOfFamily
             sizeOfVertexSet
             (St.deleteAt 0 remainingSubfamily)
             currentIndex
             (fst componentPlus)
             False

componentFunctionOfIntSet ::
     Int
  -> ISt.IntSet
  -> Integer
  -> DIMS.IntMap Bit
  -> Bool
  -> (DIMS.IntMap Bit, Bool)
componentFunctionOfIntSet sizeOfVertexSet remainingIntSubset currentIndex componentsAlreadyCalculated returnTrue
  | returnTrue = (componentsAlreadyCalculated, True)
  | remainingIntSubset == ISt.empty = (componentsAlreadyCalculated, False)
  | otherwise = do
    let minElement = ISt.findMin remainingIntSubset
    let found = DIMS.lookup minElement componentsAlreadyCalculated
    if isNothing found
      then do
        let neo =
              componentFunctionOfReversedCharVectorOfPrincipalIncreasingFamily
                sizeOfVertexSet
                minElement
                currentIndex
        let componentsUpdated =
              DIMS.insert minElement (Bit neo) componentsAlreadyCalculated
        if not neo
          then componentFunctionOfIntSet
                 sizeOfVertexSet
                 remainingIntSubset -- we do not make remainingIntSubset smaller because we are going to stop right now
                 currentIndex
                 componentsUpdated
                 True
          else componentFunctionOfIntSet
                 sizeOfVertexSet
                 (ISt.deleteMin remainingIntSubset)
                 currentIndex
                 componentsUpdated
                 False
      else if not (unBit (fromMaybe 0 found))
             then componentFunctionOfIntSet
                    sizeOfVertexSet
                    remainingIntSubset -- we do not make remainingIntSubset smaller because we are going to stop right now
                    currentIndex
                    componentsAlreadyCalculated
                    True
             else componentFunctionOfIntSet
                    sizeOfVertexSet
                    (ISt.deleteMin remainingIntSubset)
                    currentIndex
                    componentsAlreadyCalculated
                    False

componentFunctionOfReversedCharVectorOfPrincipalIncreasingFamily ::
     Int -> Int -> Integer -> Bool
componentFunctionOfReversedCharVectorOfPrincipalIncreasingFamily sizeOfVertexSet elementOfVertexSet indexForReversedCharVector =
  elementOfVertexSet `ISt.member`
  unRankLex sizeOfVertexSet indexForReversedCharVector

unRankLex :: Int -> Integer -> ISt.IntSet
unRankLex sizeOfVertexSet indexForReversedCharVector = do
  if indexForReversedCharVector == 1
    then ISt.fromList [1 .. sizeOfVertexSet]
    else do
      let layerAndRankInLayer =
            findLayerAndRankInLayer sizeOfVertexSet indexForReversedCharVector
      ISt.map
        (\x -> sizeOfVertexSet + 1 - x)
        (subsetColexUnrank
           (snd layerAndRankInLayer - snd (fst layerAndRankInLayer) - 1)
           (fst (fst layerAndRankInLayer))
           sizeOfVertexSet)

findLayerAndRankInLayer :: Int -> Integer -> ((Int, Integer), Integer)
findLayerAndRankInLayer sizeOfVertexSet indexForReversedCharVector =
  layerAndRankInLayerProcedure
    sizeOfVertexSet
    indexForReversedCharVector
    sizeOfVertexSet
    (1 :: Integer)
    (1 :: Integer)

layerAndRankInLayerProcedure ::
     Int -> Integer -> Int -> Integer -> Integer -> ((Int, Integer), Integer)
layerAndRankInLayerProcedure sizeOfVertexSet indexForReversedCharVector currentL currentShift binomialCoeff
  | indexForReversedCharVector <= currentShift =
    ((currentL, currentShift - indexForReversedCharVector), binomialCoeff)
  | otherwise = do
    let binomialCoeff =
          choose (toInteger sizeOfVertexSet) (toInteger currentL - 1)
    layerAndRankInLayerProcedure
      sizeOfVertexSet
      indexForReversedCharVector
      (currentL - 1)
      (currentShift + binomialCoeff)
      binomialCoeff

subsetColexUnrank :: Integer -> Int -> Int -> ISt.IntSet
subsetColexUnrank rankInLayer layer sizeOfVertexSet =
  subsetColexUnrankProcedure rankInLayer sizeOfVertexSet layer 1 ISt.empty

subsetColexUnrankProcedure ::
     Integer -> Int -> Int -> Int -> ISt.IntSet -> ISt.IntSet
subsetColexUnrankProcedure r x k step currentIntSet
  | step > k = currentIntSet
  | otherwise = do
    let cXbC = decreaseX r x k step
    subsetColexUnrankProcedure
      (r - snd cXbC)
      (fst cXbC)
      k
      (step + 1)
      (ISt.insert (fst cXbC + 1) currentIntSet)

decreaseX :: Integer -> Int -> Int -> Int -> (Int, Integer)
decreaseX r x k i = do
  let binomialCoeff = choose (toInteger x) (toInteger k + 1 - toInteger i)
  if binomialCoeff <= r
    then (x, binomialCoeff)
    else decreaseX r (x - 1) k i

inters :: St.Set ISt.IntSet -> ISt.IntSet -> ISt.IntSet
inters subfamily cap
  | St.size subfamily == 0 = cap
  | otherwise = do
    let fst = St.elemAt 0 subfamily
    let newcap = ISt.intersection fst cap
    inters (St.delete fst subfamily) newcap

disj :: St.Set ISt.IntSet -> ISt.IntSet -> Bool
disj subfamily cup
  | St.size subfamily == 0 = True
  | otherwise = do
    let fst = St.elemAt 0 subfamily
    ISt.disjoint fst cup &&
      (do let newcup = ISt.union fst cup
          disj (St.delete fst subfamily) newcup)
