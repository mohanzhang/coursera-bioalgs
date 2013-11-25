-- We take the straightforward approach of getting all cyclic variations of the
-- peptides, and then taking all the splits of length 0, 1, 2, ...

{-# LANGUAGE OverloadedStrings #-}

module Chapter2.TheoreticalSpectrum (parseInput, solve) where

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Set as S

data Peptide = Peptide Char | Stop deriving (Eq, Show, Ord)

type PeptideMassMap = M.Map Peptide Int

loadPeptideMassMap :: IO PeptideMassMap
loadPeptideMassMap = do
  raw <- readFile "integer_mass_table.txt"
  return $ foldl buildMap M.empty (map words $ lines raw)
  where
    buildMap acc [[p], nStr] = M.insert (Peptide p) (read nStr) acc

loadInput :: IO String
loadInput = readFile "dataset_20_3.txt"

parseInput :: String -> String
parseInput = unwords . words

solve :: String -> IO String
solve peptideStr = do
  peptideMassMap <- loadPeptideMassMap
  let peptideLength = length peptideStr
  let splits = [""] ++ (concat $ map takeSplitsOfLength [1..(peptideLength - 1)]) ++ [peptideStr]
  return . unwords $ map (show . (calcMass peptideMassMap)) splits

  where
    takeSplitsOfLength :: Int -> [String]
    takeSplitsOfLength n =
      map (\i -> take n $ drop i (L.cycle peptideStr)) [0..(length peptideStr - 1)]

    calcMass :: PeptideMassMap -> String -> Int
    calcMass peptideMassMap s = sum $ map (\p -> peptideMassMap M.! (Peptide p)) s

main :: IO ()
main = do
  input <- loadInput
  out <- solve $ parseInput input
  putStrLn out
