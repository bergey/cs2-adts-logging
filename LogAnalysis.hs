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

parsemesshelp :: String -> (Maybe MessageType, Maybe TimeStamp, Maybe String )
parsemesshelp cs = parsemess (words cs)

parsemess :: [String] -> (Maybe MessageType, Maybe TimeStamp, Maybe String )
parsemess cd  = (parseType cd, parseTimeStamp cd, parseString cd)

parseCompiler :: (Maybe MessageType, Maybe TimeStamp,Maybe String) -> LogMessage
parseCompiler (Nothing,Nothing,Just s) = Unknown s
parseCompiler (Just m, Just t,Just s) = LogMessage m t s

parseType :: [String] -> Maybe MessageType
parseType cd = if cd !! 0 == "I"
                      then Just Info
                      else if cd !! 0 == "W"
                      then Just Warning
                      else if cd !! 0 == "E"
                      then Just (Error (read(cd !! 1)))
                      else Nothing

parseTimeStamp :: [String] -> Maybe TimeStamp
parseTimeStamp cd = if elem (cd !! 2 !! 0 ) ['0','1','2','3','4','5','6','7','8','9']
                    then Just (read (cd !! 2))
                    else if elem (cd !! 1 !! 0) ['0','1','2','3','4','5','6','7','8','9']
                    then Just (read (cd !! 1))
                    else Nothing

parseString :: [String] -> Maybe String
parseString cd = if elem (cd !! 2 !! 0 ) ['0','1','2','3','4','5','6','7','8','9']
                 then Just (unwords (drop 3 cd))
                 else if elem (cd !! 1 !! 0) ['0','1','2','3','4','5','6','7','8','9']
                 then Just (unwords (drop 2 cd))
                 else Just (unwords (cd))

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
inorder (Node z (LogMessage x b n) v) = (LogMessage x b n):inorder v ++ inorder z
inorder Leaf = []

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong ((LogMessage j k l):ccs) = if k > 50
                                         then l:whatWentWrong ccs
                                         else whatWentWrong ccs
