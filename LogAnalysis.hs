module Main where

import           Log

--parseMessage :: String -> LogMessage
--parseMessage a = testParse

parseMessage :: String -> LogMessage
parseMessage (c:cs) = parsemessage (words cs)

parseType :: [String] -> MessageType
parseType (as : cd) = as

parsemessage :: [String] -> ( MessageType, TimeStamp, String )
parsemessage cd  = (parseType cd, parseTimeStamp cd, parseString cd)

parsemessagehelp :: ( MessageType, TimeStamp, String) -> LogMessage
parsemessagehelp (m,t,s) = m t s

parseTimeStamp ::
parseString ((c:as):cd) =

parseString ::
parseString ((c:as):cd) =

main :: IO ()
main = undefined
