module LogAnalysis where

import           Log
import Data.Foldable

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong ls = backToStrings(inOrder(build(checkWorth(ls))))

checkWorth :: [LogMessage] -> [LogMessage]
checkWorth ls = fmap worthIt ls

worthIt :: LogMessage -> LogMessage
worthIt l@(LogMessage (Error level) _ s) = if level >= 50
								then l
								else Unknown (show(l))
worthIt l@(LogMessage _ _ _) = Unknown (show(l))
worthIt (Unknown s) = Unknown s

backToStrings :: [LogMessage] -> [String]
backToStrings xs = fmap show xs

inOrder :: MessageTree -> [LogMessage]
inOrder (Node a t b) = inOrder a ++ [t] ++ inOrder b
inOrder Leaf = []

build :: [LogMessage] -> MessageTree
build xs = foldr insert Leaf xs

--addin :: LogMessage -> MessageTree -> MessageTree
--addin xs Leaf = (Node Leaf xs Leaf)
--addin xs y = insert xs y

insert :: LogMessage -> MessageTree -> MessageTree
insert x y = putIn x y

putIn :: LogMessage -> MessageTree -> MessageTree
putIn (Unknown _) x = x
putIn nlog Leaf = (Node Leaf nlog Leaf)
putIn nlog@(LogMessage a new c) (Node ma l mb) = if lessMove new l
                        then (Node (putIn nlog ma) l mb)
                        else (Node ma l (putIn nlog mb))
							
--need to add in message trees to functions below
moreMove :: TimeStamp -> LogMessage -> Bool
moreMove new (LogMessage _ check _ )= if new > check
									then True
									else False

lessMove :: TimeStamp -> LogMessage -> Bool
lessMove new (LogMessage _ check _ )= if new < check
									then True
									else False


--ifLessThan :: String -> [String] -> String
--ifLessThan (l, []) = (l, [])
--ifLessThan (l, (s:ss)) = if l < s
--						then ifLessThan l ss
--						else ifLessThan s ss

takeAllParse :: String -> [LogMessage]
takeAllParse s = map parseMessage (lines s)

parseMessage :: String -> LogMessage
parseMessage (c:cs) = if checkMessageType c == Nothing
	then Unknown (c:cs)
	else combineToLog (c:cs) (checkMessageType c) (checkTimeStamp (ifError(c:cs)))

checkMessageType :: Char -> Maybe MessageType
checkMessageType c = if c == 'I'
					then Just Info
					else if c == 'W'
					then Just Warning
					else if c == 'E'
					then Just (Error 0)
					else Nothing

--if it is an error, it runs takeoutint on it to get the level
--otherwise it returns 0, the ones with 0 will have the 0 taken out later
--returns the string, with the error level taken out if aplicable
ifError :: String -> (Int, String)
ifError (c:cs) = if c == 'E'
				then takeOutInt (words cs)
				else (0, cs)

--takes out the first thing in the string as the int
--only run if it is an error to take out the 
--error severity, and returns the rest of the string
takeOutInt :: [String] -> (Int, String)
takeOutInt (c:cs) = (read c, unwords cs)

--just runs readtimestamp and keeps the int for in case it is an
--error and it returned an important error level there
--takes out the timestamp and returns the rest of the string
checkTimeStamp :: (Int,String) -> (Int, (TimeStamp, String))
checkTimeStamp (i,s) = (i, readTimeStamp (words(s)))

--takes a list of strings and uses the first character (already
--without message type) and makes that the time stamp, then returns
--the rest of the string
readTimeStamp :: [String] -> (TimeStamp, String)
readTimeStamp (s:ss) = (read (s), unwords ss)

--Combines inputs of the components of a log message into a LogMessage
combineToLog :: String -> Maybe MessageType -> (Int, (TimeStamp, String)) -> LogMessage
combineToLog origs(Just m) (0, (t, s)) = LogMessage m t s
combineToLog origs (Just m) (i, (t, s)) = LogMessage (Error i) t s
combineToLog origs Nothing (i, (t,s)) =  Unknown origs