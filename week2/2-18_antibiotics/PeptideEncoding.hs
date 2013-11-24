-- We are given a peptide string, so we generate all the genomic substrings and
-- scan for those

{-# LANGUAGE OverloadedStrings #-}

module Chapter2.PeptideEncoding (parseInput, solve) where

import Control.Applicative ((<$>))
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Set as S

-- user required to enforce invariant length(Codon) == 3
type Codon = String

data Peptide = Peptide Char | Stop deriving (Eq, Show, Ord)
type PeptideCodonMap = M.Map Peptide [Codon]

type Genome = T.Text

loadPeptideCodonMap :: IO PeptideCodonMap
loadPeptideCodonMap = do
  raw <- readFile "RNA_codon_table_2.txt"
  return $ foldl buildMap M.empty (map words $ lines raw)
  where
    buildMap acc [codon, [peptide]] = M.insertWith (++) (Peptide peptide) [codon] acc
    buildMap acc [codon] = M.insertWith (++) Stop [codon] acc

loadInput :: IO String
loadInput = readFile "dataset_18_6.txt"

parseInput :: String -> (Genome, [Peptide])
parseInput raw = let [g, p] = lines raw in (T.pack g, (map Peptide $ p))

solve :: (Genome, [Peptide]) -> IO String
solve (genome, peptides) = do
  peptideCodonMap <- loadPeptideCodonMap
  let candidates = addReverseComplements $ genomifyPeptides peptideCodonMap peptides
  let found = foldl (\acc i ->
                let window = T.take (3*(length peptides)) $ T.drop i genome in
                if window `S.member` candidates then window:acc else acc
              ) [] [0..(T.length genome - 3*(length peptides))]
  return . unlines . (map T.unpack) $ L.nub found

main :: IO ()
main = do
  input <- loadInput
  out <- solve $ parseInput input
  putStrLn out

-- Take a peptide string and return a set of genome substrings that represent the
-- peptide
genomifyPeptides :: PeptideCodonMap -> [Peptide] -> S.Set Genome
genomifyPeptides peptideCodonMap ps =
  S.fromList $ map (T.pack . concat) $ sequence $ map (\p -> peptideCodonMap M.! p) ps

-- take a set of genomic substrings and add the reverse complements to the set
addReverseComplements :: S.Set Genome -> S.Set Genome
addReverseComplements s = S.foldr (\g acc -> S.insert (reverseComplement g) (S.insert g acc)) S.empty s

reverseComplement :: T.Text -> T.Text
reverseComplement = T.reverse . complement

complement :: T.Text -> T.Text
complement "" = ""
complement g = case T.head g of
  'C' -> 'G' `T.cons` (complement $ T.tail g)
  'G' -> 'C' `T.cons` (complement $ T.tail g)
  'A' -> 'T' `T.cons` (complement $ T.tail g)
  'T' -> 'A' `T.cons` (complement $ T.tail g)
