module Main where

import           Log

main :: IO ()
main = undefined

parseMessage :: String -> LogMessage
parseMessage (a:as) =  parseMessage 
parseMessage a = 	
parse :: String -> [LogMessage]