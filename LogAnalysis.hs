module Main where

import Log

main :: IO ()
main = testWhatWentWrong parse whatWentWrong (sample.log)

parseMessage :: String -> LogMessage
parseMessage s | head(s)=='I' = LogMessage Info (read ((words s) !! 1)) (unwords (drop 1 (words s)))
parseMessage s | head(s)=='W' = LogMessage Warning (read ((words s) !! 1)) (unwords (drop 1 (words s)))
parseMessage s | head(s)=='E' = LogMessage (Error (read ((words s) !! 1))) (read ((words s) !! 2)) (unwords (drop 2 (words s)))
parseMessage s = Unknown s

parse :: String -> [LogMessage]
parse s = map (parseMessage) (lines s)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown l) m = m
insert l Leaf = Node Leaf l Leaf
insert (LogMessage t ts s) (Node mt1 (LogMessage tn tsn sn) mt2) | ts>tsn = Node mt1 (LogMessage t ts s) (insert (LogMessage t ts s) mt2)
insert (LogMessage t ts s) (Node mt1 (LogMessage tn tsn sn) mt2) = Node (insert (LogMessage t ts s) mt1) (LogMessage t ts s) mt2

build :: [LogMessage] -> MessageTree
build l = foldr (insert) Leaf l

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node mt1 lm mt2) = inOrder mt1 ++ [lm] ++ inOrder mt2

helper :: LogMessage -> String
helper (LogMessage t ts s) = show(t) ++ show(ts) ++ s 

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [LogMessage t ts s] = map (helper) (filter (t==(Error Int) && t>50) (inOrder (build [LogMessage t ts s])))

--Filter so only messages that are errors with severity of 50 or greater are retained