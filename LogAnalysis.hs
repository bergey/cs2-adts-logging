module Main where

import           Log

main :: IO ()
main = print (parseMessage "E 2 562 help help")

takeAllParse :: [String] -> [LogMessage]
takeAllParse s = map parseMessage s

parseMessage :: String -> LogMessage
parseMessage (c:cs) = combineToLog (c:cs) (checkMessageType c) (checkTimeStamp (ifError(c:cs)))

checkMessageType :: Char -> Maybe MessageType
checkMessageType c = if c == 'I'
					then Just Info
					else if c == 'W'
					then Just Warning
					else if c == 'E'
					then Just (Error 0)
					else Nothing

ifError :: String -> (Int, String)
ifError (c:cs) = if c == 'E'
				then takeOutInt (words cs)
				else (0, cs)

takeOutInt :: [String] -> (Int, String)
takeOutInt (c:cs) = (read c, unwords cs)

checkTimeStamp :: (Int,String) -> (Int, (TimeStamp, String))
checkTimeStamp (i,s) = (i, readTimeStamp (words(s)))

readTimeStamp :: [String] -> (TimeStamp, String)
readTimeStamp (s:ss) = (read (s), unwords ss)

combineToLog :: String -> Maybe MessageType -> (Int, (TimeStamp, String)) -> LogMessage
combineToLog origs(Just m) (0, (t, s)) = LogMessage m t s
combineToLog origs (Just m) (i, (t, s)) = LogMessage (Error i) t s
combineToLog origs Nothing (i, (t,s)) =  Unknown origs