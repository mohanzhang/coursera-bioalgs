-- Switching over to haskell because these problems are getting more
-- data-structure intensive

-- For this clumps problem, where we have to find all distinct k-mers forming
-- (L,t)-clumps, we first find all distinct k-mers, then ask which ones are
-- in (L,t)-clumps.

module Chapter1 where

import qualified Data.Text as T
import Data.Text.Read
import qualified Data.List as L

import Clumps

readInput :: IO (T.Text, Int, Int, Int)
readInput = do
  raw <- readFile "stepic_dataset.txt"
  let (s, [k, l, t]) = (head . T.lines $ T.pack raw, (map read) . words . last . lines $ raw)
  return (s, k, l, t)

main :: IO ()
main = do
  (s, k, l, t) <- readInput
  putStrLn . T.unpack . T.unwords . L.nub $ clumps s k l t
  return ()
