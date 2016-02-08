module LogAnalysis where

import Log

parseMessagehelper::String->[String]
parseMessagehelper a = words a

parseMessagehelper2:: [String]->String
parseMessagehelper2 a = unwords a 

charToString :: Char -> String
charToString c = [c]

errormessagetypes::[String]->MessageType 
errormessagetypes (a:rest) | a!!0 == 'I' = Info
					| a!!0 == 'W' = Warning
					| a!!0 == 'E' && b<100 = Error b where
						b = read (rest!!0)

checkmessagemessagetype::[String]->String
checkmessagemessagetype (c:cs) | errormessagetypes(c:cs) == Info = cs!!0
							   | errormessagetypes(c:cs) == Warning = cs!!0   
							   | otherwise = cs!!1

dropthisamount::[String]->Int 
dropthisamount (c:cs) = case errormessagetypes(c:cs) of 
	Error b  -> 3 
	otherwise ->2 

parseMessage :: String -> LogMessage
parseMessage a = LogMessage (errormessagetypes(untranslatederror)) (read (checkmessagemessagetype(untranslatederror))) (parseMessagehelper2 (drop (dropthisamount(untranslatederror)) untranslatederror)) where
	untranslatederror = (parseMessagehelper a) 

parse :: String -> [LogMessage]
parse a = map parseMessage (lines a)

data MessageTree = Leaf
 Node MessageTree LogMessage MessageTree

insert :: LogMessage -> MessageTree -> MessageTree
insert a = Node MessageTree a MessageTree
insert = Leaf Leaf


build :: [LogMessage] -> MessageTree
build a = map insert a

--inOrder :: MessageTree -> [LogMessage]

