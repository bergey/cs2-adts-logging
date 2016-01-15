module Main where

import           Log

import Data.Foldable

main :: IO ()
main = putStrLn =<< tests
--main = traverse_ putStrLn =<< test

tests = fmap unlines test


test = testWhatWentWrong parse whatWentWrong "error.log"

parse :: String -> [LogMessage]
parse q = map parseMessage (lines q)





parseMessage :: String -> LogMessage
parseMessage c = (parsemessage(parsestamp (parseType c)))

parseType :: String -> (Maybe MessageType, String)
parseType (c : cd) = if (c == 'I')
					 then (Just Info, cd)
					 else if (c == 'W')
					 	  then (Just Warning, cd)
					 	  else if (c == 'E')
					 	  then er cd 
					 	  else (Nothing, cd)

er :: String -> (Maybe MessageType, String)
er cd = (Just (Error (read (head (words cd)))), unwords (tail (words cd)))
--er cd = (Just (Error 50), cd)


parsestamp :: (Maybe MessageType, String) -> (Maybe MessageType, TimeStamp, String)
parsestamp (Nothing, cd) = (Nothing ,0 ,cd)
--parsestamp (e, cd) = (e, 1, unwords(nf (words cd)))
parsestamp (e, cd) = (e, read (head (words cd)) , unwords (tail (words cd)))

parsemessage :: (Maybe MessageType, TimeStamp, String) -> LogMessage
parsemessage (Nothing, n, c) = Unknown c
parsemessage (Just a, n, s) = LogMessage a n s

getstamp :: LogMessage -> Int
getstamp (LogMessage a t g) = t

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown s) q = q
insert s (Node a n q) = if ((getstamp s) < (getstamp n))
	                    then Node (insert s a) n q 
	                    else Node a n (insert s q)
insert s Leaf = Node Leaf s Leaf

build :: [LogMessage] -> MessageTree
build ms = builder (ms, Leaf)

builder :: ([LogMessage], MessageTree) -> MessageTree
builder ([], a) =  a
builder ((m : ml), a) = builder (ml, insert m a)

inOrder :: MessageTree -> [LogMessage]
inOrder (Node Leaf n q) = n : inOrder q
inOrder (Node a n q) = inOrder a ++ inOrder (Node Leaf n q)
inOrder Leaf = []  

relevant :: [LogMessage] -> [String]
relevant ((LogMessage (Error n) r q) : lm) = if (n >  50)
							    			then q : relevant lm
							    			else relevant lm
relevant ((Unknown c) : lm) = c : relevant lm
relevant (a : lm) = relevant lm
relevant [] = []


whatWentWrong :: [LogMessage] -> [String]
whatWentWrong a = relevant (inOrder (build a))







