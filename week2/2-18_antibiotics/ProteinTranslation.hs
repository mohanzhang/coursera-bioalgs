-- Starting in Chapter 2, I will be more disciplined about using custom
-- datatypes to make the code clearer. I didn't think Chapter 1 would end up
-- getting so complicated as far as data manipulations went.
--
-- As for this particular problem, we are basically mapping a lookup over the
-- input string.

module Chapter2.ProteinTranslation (solve) where

import Control.Applicative ((<$>))
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.List as L

-- user required to enforce invariant length(Codon) == 3
type Codon = String

data Peptide = Peptide Char | Stop deriving (Eq, Show)
type CodonPeptideMap = M.Map Codon Peptide

type Genome = T.Text

loadCodonPeptideMap :: IO CodonPeptideMap
loadCodonPeptideMap = do
  raw <- readFile "RNA_codon_table_1.txt"
  return $ foldl buildMap M.empty (map words $ lines raw)
  where
    buildMap acc [codon, [peptide]] = M.insert codon (Peptide peptide) acc
    buildMap acc [codon] = M.insert codon Stop acc


loadInput :: IO Genome
loadInput = T.pack <$> readFile "dataset_18_3.txt"

stringifyPeptides :: [Peptide] -> String
stringifyPeptides (Stop:_) = []
stringifyPeptides ((Peptide p):ps) = p:(stringifyPeptides ps)

solve :: T.Text -> IO String
solve input = do
  codonPeptideMap <- loadCodonPeptideMap
  let inCodons = map T.unpack $ T.chunksOf 3 input
  -- fromJust will blow up if the codon doesn't exist, but that's intentional
  let peptides = map (\c -> fromJust $ M.lookup c codonPeptideMap) inCodons
  let iFirstStop = fromJust $ Stop `L.elemIndex` peptides
  return $ stringifyPeptides $ take (1 + iFirstStop) peptides

main :: IO ()
main = do
  input <- loadInput
  out <- solve input
  putStrLn out
