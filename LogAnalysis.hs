module Main where

import           Log

main :: IO ()
main = undefined

MaybeMessageType a = Nothing | Just

parseMessageHelper :: [String] -> (Maybe MessageType , String)
parseMessageHelper x:n:xs = if x == "I"
						then (Just Info , xs)
						else if x == "E"
						then (Just (Error (read n)) , xs) 
						else if x == "W"
						then (Just Warning , xs)
						else Nothing


