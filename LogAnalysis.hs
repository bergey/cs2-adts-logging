module Main where

import           Log

--parseMessage :: String -> LogMessage
--parseMessage a = testParse

--parse :: String -> [LogMessage]
--parse cs =

parseMessage :: String -> LogMessage
parseMessage cs = parseCompiler (parsemesshelp cs)

parsemesshelp :: String -> ( MessageType, TimeStamp, String )
parsemesshelp (c:cs) = parsemess (words cs)

parsemess :: [String] -> ( MessageType, TimeStamp, String )
parsemess cd  = (parseType cd, parseTimeStamp cd, parseString cd)

parseCompiler :: (MessageType, TimeStamp, String) -> LogMessage
parseCompiler (m,t,s) = LogMessage m t s

parseType :: [String] -> MessageType
parseType (as : cd) = if cd !! 1 == "I"
                      then Info
                      else if cd !! 1 == "W"
                      then Warning
                      else Error (read(cd !! 2 ))

parseTimeStamp :: [String] -> TimeStamp
parseTimeStamp (as:cd) = read( cd !! 2 )

parseString :: [String] -> String
parseString (as:cd) = cd !! 3

main :: IO ()
main = undefined
