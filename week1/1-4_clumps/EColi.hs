module Main where

import qualified Data.Text as T
import Data.Text.IO (readFile)
import qualified Data.List as L

import Clumps

main :: IO ()
main = do
  s <- Data.Text.IO.readFile "E-coli.txt"
  let (k, l, t) = (9, 500, 3)
  putStrLn . show . length . L.nub $ clumps s k l t
  return ()
