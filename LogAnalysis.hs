module LogAnalysis where

import           Log


import           Control.Arrow
import           Data.Char
import           Data.Foldable
import           Data.List     (span)
import           Data.Maybe

parse :: String -> [LogMessage]
parse = undefined

parseMessage :: String -> LogMessage
parseMessage msg = undefined

parseInt :: String -> Maybe (Int, String)
parseInt s = undefined

parseMessageType :: String -> Maybe MessageType
parseMessageType = undefined

parseType :: String -> Maybe (MessageType, String)
parseType = undefined

insert :: LogMessage -> MessageTree -> MessageTree
insert _ t = undefined

build :: [LogMessage] -> MessageTree
build = undefined

inOrder :: MessageTree -> [LogMessage]
inOrder = undefined

serious :: LogMessage -> Bool
serious _ = undefined

msg :: LogMessage -> String
msg = undefined

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = undefined
