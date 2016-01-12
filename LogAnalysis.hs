module Main where

import           Log

--parseMessage :: String -> LogMessage
--parseMessage a = testParse

parseMessage :: String -> LogMessage
parseMessage (c:cs) = parsemessage (words cs)

parsemessage :: [String] -> ( MessageType, TimeStamp, String )
parsemessage cd  = (parseType cd, parseTimeStamp cd, parseString cd)

parsemessagehelp :: (MessageType, TimeStamp, String) -> LogMessage
parsemessagehelp (m,t,s) = m t s

parseType :: [String] -> MessageType
parseType (as : cd) = as

parseTimeStamp :: [String] -> TimeStamp
parseTimeStamp (as:cd) = cd !! 2

parseString :: [String] -> String
parseString (as:cd) = cd !! 3

main :: IO ()
main = undefined
