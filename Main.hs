module Main where

import Log
import           LogAnalysis

import           Data.Foldable

main :: IO ()
main = traverse_ putStrLn =<<
  testWhatWentWrong parse (map msg . inOrder . build) "error.log"
