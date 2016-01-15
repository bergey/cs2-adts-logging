module Main where

import           Log

main :: IO ()
main = undefined

parseMessagehelper::String->[String]
parseMessagehelper a = words a

parseMessagehelper2:: [String]->String
parseMessagehelper2 a = unwords a 

parseMessage :: String -> LogMessage
parseMessage a = LogMessage (untranslatederror!! 0) (untranslatederror!! 1) (untranslatederror!! 2) (parseMessagehelper2 untranslatederror) where
	untranslatederror = (parseMessagehelper a) 

--parse :: String -> [LogMessage]