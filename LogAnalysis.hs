module Main where

import           Log

main :: IO ()
main = undefined

parseMessageMessageType :: [String] -> (Maybe MessageType , String)
parseMessageMessageType (x:n:xs) = if x == "I"
						then (Just Info , unwords xs) 
						else if x == "E"
						then (Just (Error (read n)) , unwords xs) 
						else if x == "W"
						then (Just Warning , unwords xs)
						else (Nothing, unwords xs)

parseMessageTimeStamp :: [String] -> (Maybe MessageType , TimeStamp, String)
parseMessageTimeStamp (as) = (m, (read w), unwords ws) where
	(m,xs) = parseMessageMessageType (as)
	(w:ws) = words xs

parseMessage :: String -> LogMessage
parseMessage (errorLine) = parseMessageTimeStamp (words errorLine)

parse :: String -> [LogMessage]
parse (errorlog) = map (parseMessage) (lines errorlog)
 
