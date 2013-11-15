-- Naively, we would generate all the possible k-mers and then send it through
-- our previous approx pattern finder algorithm, but that would probably take
-- forever (ok, I know it takes forever because I tried it).
-- SOOOO... instead it would make more sense to look at each k-mer that appears
-- in the genome and tally up all the mutations that it can represent, e.g.
-- ATGATGATG for 3-mers with 1 mismatch would look at ATG and determine that it
-- now means that ATG, TTG, CTG, GTG, AGG, etc. are "in play."
-- We then add 1 to each k-mer that is in play, and at the end, we reduce the
-- map down to those that have the highest counts. These are then the most
-- frequent k-mers.

module Chapter1 where

import Data.Array
import qualified Data.List as L
import qualified Data.List.Utils as L
import qualified Data.Map as M

import Debug.Trace

readInput :: IO (String, Int, Int)
readInput = do
  raw <- readFile "dataset_8_4.txt"
  let [genome, k, d] = words raw --words "ACGTTGCATGTCGCATGATGCATGAGAGCT 4 1"
  return (genome, read k, read d)


freqKmers :: String -> Int -> Int -> [String]
freqKmers genome k d = snd . maximum . L.flipAL $ M.toList $ foldl tally M.empty [0..(length genome) - k - 1]
  where
    -- accumulator
    tally :: M.Map String Int -> Int -> M.Map String Int
    tally counts i =
      let kmer = (take k) . (drop i) $ genome in
      foldl (\acc x -> M.insertWith (+) x 1 acc) counts (deviations kmer d)

    -- all k-mers with d deviations from the given kmer
    -- we do this by building a bitstring of the possible deviation locations
    -- and inserting the deviations there
    deviations :: String -> Int -> [String]
    deviations kmer d =
      -- generate all the deviation locations and go through each one
      foldl (\acc devArray ->
          -- for this particular deviation sequence, we generate all the new
          -- bases and cross all the possibilities, e.g.
          -- [A     [A]    [T     [G]
          --  T  x       x  G]  x
          --  G
          --  C]
          -- == [AATG, AAGG, TATG, etc]
          (sequence $ map (uncurry performDeviation) $ zip devArray kmer) ++ acc
        ) [] (deviationMatrix k d)

    performDeviation :: Bool -> Char -> [Char]
    performDeviation True b = [b]
    performDeviation False 'A' = "CTG"
    performDeviation False 'T' = "ACG"
    performDeviation False 'C' = "ATG"
    performDeviation False 'G' = "ATC"

    -- A list of T/F markers for a kmer that represents whether the individual bases should
    -- stay the same in the mutation, e.g.
    -- deviating ATG by T T F would mean ATT, ATA, ATC
    deviationMatrix :: Int -> Int -> [[Bool]]
    deviationMatrix k 0 = [take k $ repeat True]
    deviationMatrix k n = L.nub $ L.permutations ((take (k-n) $ repeat True) ++ (take n $ repeat False)) ++ (deviationMatrix k (n-1))


main :: IO ()
main = do
  (genome, k, d) <- readInput
  putStrLn . show $ freqKmers genome k d

{-
  (genome, k, d) <- readInput
  let all_kmers = genKmers k
  let descCandidates = reverse . L.sort $ map (\kmer -> (approxMatchOccurrences kmer genome d, kmer)) all_kmers
  let maxMatch = fst . head $ descCandidates
  putStrLn . unwords . (map snd) $ takeWhile (\(x, _) -> x == maxMatch) descCandidates


genKmers :: Int -> [String]
genKmers 1 = ["C", "G", "A", "T"]
genKmers n =
  let suffixes = genKmers (n-1) in
  foldl (\acc suffix -> ('C':suffix):('G':suffix):('A':suffix):('T':suffix):acc) [] suffixes

approxMatchOccurrences :: String -> String -> Int -> Int
approxMatchOccurrences kmer genome d =
  let k = length kmer
      kmers = map (\i -> (i, take k (drop i genome))) [0..((length genome) - (length kmer))]
      approx_matches = foldl (\acc (i,x) -> if difference x kmer <= d then i:acc else acc) [] kmers
  in length approx_matches

-- assume a and b are equal length: zip em up, compare, and collect & count Falses
difference :: String -> String -> Int
difference a b = length $ filter not $ map (uncurry (==)) $ zip a b
-}
