-- The algorithm is already given to us, so we just follow along: we grow the
-- set at each step with every possible addition, and then prune to just those
-- that are still consistent with the spectral data we received as input

{-# LANGUAGE OverloadedStrings #-}

module Chapter2.CyclopeptideSequencing (parseInput, solve) where

import Control.Applicative ((<$>))
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Set as S
import Data.String.Utils (join)

import Debug.Trace

peptideMasses :: [Int]
peptideMasses = [57, 71, 87, 97, 99, 101, 103, 113, 114, 115, 128, 129, 131, 137, 147, 156, 163, 186]

loadInput :: IO String
loadInput = readFile "dataset_22_4.txt"

parseInput :: String -> [Int]
parseInput = (map read) . words

solve :: [Int] -> IO String
solve masses = do
  return . unwords $ map (\p -> join "-" $ map show p) $ branchAndPrune [[]]

  where
    branchAndPrune :: [[Int]] -> [[Int]]
    branchAndPrune peptides =
      let nextSet = prune $ branch peptides in
      traceShow nextSet $
      if length nextSet == 0 then peptides else
      case (length (head nextSet) `compare` length (head peptides)) of
        EQ -> peptides
        GT -> branchAndPrune nextSet
        LT -> peptides

    branch :: [[Int]] -> [[Int]]
    branch peptides = foldl (\acc p -> acc ++ map (\x -> p ++ [x]) peptideMasses) [] peptides

    -- check whether each peptide is consistent
    prune :: [[Int]] -> [[Int]]
    prune peptides = foldl keepIfConsistent [] peptides

    keepIfConsistent :: [[Int]] -> [Int] -> [[Int]]
    keepIfConsistent consistentPeptides peptide =
      let consistent = foldl (\acc (x,y) ->
                                acc && (x `elem` masses) && (y `elem` masses)
                            ) True (summedSplits peptide) in
      if consistent then peptide:consistentPeptides else consistentPeptides

    summedSplits :: [Int] -> [(Int, Int)]
    summedSplits peptide =
      map (\i -> let (a,b) = L.splitAt i peptide in (sum a, sum b)) [0..(length peptide - 1)]

main :: IO ()
main = do
  input <- loadInput
  out <- solve $ parseInput input
  putStrLn out

