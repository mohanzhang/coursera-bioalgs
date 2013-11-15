-- To find approximate patterns, we can gather every k-mer from the genome and
-- compare it to our target, noting where it is different

module Chapter1 where

import Data.Array
import qualified Data.List as L

readInput :: IO (String, String, Int)
readInput = do
  raw <- readFile "dataset_8_3.txt"
  let [kmer, genome, d] = lines raw
  return (kmer, genome, read d)


main :: IO ()
main = do
  (kmer, genome, d) <- readInput
  let k = length kmer
  let kmers = map (\i -> (i, take k (drop i genome))) [0..((length genome) - (length kmer))]
  let approx_matches = foldl (\acc (i,x) -> if difference x kmer <= d then i:acc else acc) [] kmers
  putStrLn . unwords . (map show) . reverse $ approx_matches

-- assume a and b are equal length: zip em up, compare, and collect & count Falses
difference :: String -> String -> Int
difference a b = length $ filter not $ map (uncurry (==)) $ zip a b
