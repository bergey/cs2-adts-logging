module LogInstances (isSorted) where
-- Export isSorted and instances, not 'build'

import Log
import Test.Tasty.QuickCheck

import Data.List (sortOn)

-- The library provides Arbitrary instances for built-in types like Int & String
-- This module tells QuickCheck how to pick an arbitrary value of our new types.

instance Arbitrary MessageType where
  arbitrary = oneof
    [pure Info
    , pure Warning
    , fmap Error arbitrary ]  -- An Error with an arbitrary Int

-- This instance never makes Unknown messages
instance Arbitrary LogMessage where
  arbitrary = LogMessage <$> arbitrary <*> arbitrary <*> arbitrary

-- This instance always makes sorted MessageTrees
instance Arbitrary MessageTree where
  arbitrary = fmap (build . sortOn (\(LogMessage _ t _) -> t) ) arbitrary

build :: [LogMessage] -> MessageTree
build [] = Leaf
build msgs = let (l,(x:r)) = splitAt (length msgs `div` 2) msgs in
  Node (build l) x (build r)

isSorted :: [LogMessage] -> Bool
isSorted [] = True
isSorted [_] = True
isSorted (LogMessage _ t1 _ : LogMessage _ t2 _ : rest)
  = t1 <= t2 && isSorted rest
