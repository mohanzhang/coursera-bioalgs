-- Conceptually, we are just folding the genome to a list of integers
-- representing the skew at each position

module Chapter1 where

import qualified Data.List as L

readInput :: IO (String)
readInput = do
  raw <- readFile "dataset_7_6.txt"
  return raw

printSkew :: String -> String
printSkew = unwords . (map show) . skews

-- printSkew "GAGCCACCGCGATA"

main :: IO ()
main = do
  genome <- readInput
  putStrLn . unwords . (map show) $ minSkews genome


skews :: String -> [Int]
skews genome = reverse $ foldl (\(n:acc) x -> (case x of
        'C' -> n - 1
        'G' -> n + 1
        _ -> n):n:acc) [0] genome

minSkews :: String -> [Int]
minSkews genome =
  let sortedPairs = L.sort $ zip (skews genome) [0..] in
  let minValue = fst . head $ sortedPairs in
  map snd $ takeWhile (\(s,_) -> s == minValue) sortedPairs
