-- Takes a solution and a test dataset and checks the output.
-- Solutions must expose Solve :: T.Text -> IO String

module Validate where

import qualified Data.Text as T

check :: (String -> a) -> (a -> IO String) -> String -> IO ()
check inputParser f dataFile = do
  raw <- readFile dataFile
  let inputBlock = unlines $ takeWhile ((/=) "Output") $ drop 1 $ lines raw
  let expectedOut = unlines . reverse $ takeWhile ((/=) "Output") $ reverse . lines $ raw
  actualOut <- f $ inputParser inputBlock
  if (unwords . words $ actualOut) == (unwords . words $ expectedOut)
    then putStrLn "Pass"
    else putStrLn ("Expected: " ++ expectedOut) >> putStrLn ("But got:  " ++ actualOut)
