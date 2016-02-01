module LogAnalysis where

import           Log

--parseMessage :: String -> LogMessage
--parseMessage a = testParse

parse :: String -> [LogMessage]
parse cs = parseStuff (lines cs)

parseStuff :: [String] -> [LogMessage]
parseStuff cs = map parseMessage cs

parseMessage :: String -> LogMessage
parseMessage cs = parseCompiler (parsemesshelp cs)

parsemesshelp :: String -> ( MessageType, TimeStamp, String )
parsemesshelp cs = parsemess (words cs)

parsemess :: [String] -> ( MessageType, TimeStamp, String )
parsemess cd  = (parseType cd, parseTimeStamp cd, parseString cd)

parseCompiler :: (MessageType, TimeStamp, String) -> LogMessage
parseCompiler (m,t,s) = LogMessage m t s

parseType :: [String] -> MessageType
parseType cd = if cd !! 0 == "I"
                      then Info
                      else if cd !! 0 == "W"
                      then Warning
                      else Error (read(cd !! 1))

parseTimeStamp :: [String] -> TimeStamp
parseTimeStamp cd = if elem (cd !! 2 !! 0 ) ['0','1','2','3','4','5','6','7','8','9']
                    then read (cd !! 2)
                    else read( cd !! 1 )

parseString :: [String] -> String
parseString cd = if elem (cd !! 2 !! 0 ) ['0','1','2','3','4','5','6','7','8','9']
                 then unwords (drop 3 cd)
                 else unwords (drop 2 cd)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) as = as
insert (LogMessage j k l) (Node z (LogMessage x b n) v) = if k > b
                                                          then Node Leaf (LogMessage j k l ) (Node z (LogMessage x b n) v)
                                                          else Node (Node z (LogMessage x b n) v) (LogMessage j k l) Leaf
insert (LogMessage j k l) Leaf = Node Leaf (LogMessage j k l) Leaf

build:: [LogMessage] -> MessageTree
build [] = Leaf
build (c:ccs) = insert (c) (build ccs)

inorder :: MessageTree -> [LogMessage]
inorder (Node z (LogMessage x b n) v) = ((LogMessage x b n):(inorder (Node z(LogMessage x b n)v)))
inorder Leaf = []

--whatWentWrong :: [LogMessage] -> [String]
--whatWentWrong ((LogMessage j k l):ccs) = if k > 50
--                                       then l
--                                       else ccs

main :: IO ()
main = undefined
