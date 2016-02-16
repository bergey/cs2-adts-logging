-- This is how we import only a few definitions from Test.Tasty
import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Log
import LogInstances

-- import everything *except* `main` from LogAnalysis
import LogAnalysis hiding (main)

tests :: TestTree
tests = testGroup "unit tests"
  [ testCase "parseMessage Info"
    ( parseMessage "I 6 Completed armadillo processing" @?=
      LogMessage Info 6 "Completed armadillo processing" )
    -- If you don't have a function called praseMessage, change the
    -- test to match your code.

    -- Add at least 3 more test cases for 'parseMessage', including
    -- one with an error, one with a warning, and one with an Unknown
    , testCase "parseMessage Error"
        ( parseMessage "E 2 6 This went wrong" @?=
        LogMessage (Error 2) 6 "This went wrong" )
    , testCase "parseMessage Warning"
        ( parseMessage "W 6 This is a warning" @?=
        LogMessage Warning 6 "This is a warning" )
    , testCase "parseMessage Unknown"
        ( parseMessage "Unknown a thing happened" @?=
        Unknown "a thing happened" )

    -- We should also test the smaller parts.  Change the test below
    -- to match the code you actually wrote.
    , testCase "parseMessageMessageType I"
        ( parseMessageMessageType "I 6 Completed armadillo processing"
        @?= (Just Info, "6 Completed armadillo processing"))

    -- Add at least 3 more tests for MessageType parsing in isolation.
    , testCase "parseMessageMessageType E"
        ( parseMessageMessageType "E 2 6 Completed armadillo processing"
        @?= (Just (Error 2), "6 Completed armadillo processing"))
    , testCase "parseMessageMessageType W"
        ( parseMessageMessageType "W 6 Completed armadillo processing"
        @?= (Just Warning, "6 Completed armadillo processing"))
    , testCase "parseMessage Unknown"
        ( parseMessageMessageType "Unknown a thing happened"
        @?= (Nothing, "a thing happened" ))
    -- Add tests for timestamp parsing.  Think in particular about
    -- what the function does if the input doesn't start with a digit,
    -- or has some spaces followed by digits.

    , testCase "parseMessageTimeStamp I"
        ( parseMessageMessageType "I 6 Completed armadillo processing"
        @?= (Just Info, 6, "Completed armadillo processing"))
    , testCase "parseMessageTimeStamp E"
        ( parseMessageMessageType "E 2 6 Completed armadillo processing"
        @?= (Just (Error 2), 6, "Completed armadillo processing"))
    , testCase "parseMessageTimeStamp W"
        ( parseMessageMessageType "W 6 Completed armadillo processing"
        @?= (Just Warning, 6, "Completed armadillo processing"))

    -- How many tests do you think is enough?  Write at least 3
    -- sentences explaining your decision.
        -- I think that for ParseMessage 3 test is enough because I only use 3
        -- functions to parse message. I think that there should be a test for
        -- every function used because it helps you locate the problem in the
        -- code.
    -- Write at least 5 tests for 'insert', with sufficiently
    -- different inputs to test most of the cases.  Look at your code
    -- for 'insert', and any bugs you ran into while writing it.

    , testCase "insert info Leaf"
        ( insert (LogMessage Info 6 "Completed armadillo processing"), Leaf
        @?= (Node Leaf (LogMessage Info 6 "Completed armadillo processing") Leaf ))

    , testCase "insert error leaf"
        ( insert (LogMessage (Error 2) 6 "Completed armadillo processing") Leaf
        @?= (Node Leaf (LogMessage (Error 2) 6 "Completed armadillo processing") Leaf ))

    , testCase "insert info Node"
        ( insert (LogMessage Info 6 "Completed armadillo processing") (Node (Node Leaf (parseMessage "I 2 sdgjh ash") Leaf) (parseMessage "I 5 sdgjh ash") (Node Leaf  (parseMessage "I 13 sdgjh ash") Leaf ))
        @?= (Node (Node Leaf (LogMessage Info 2 "sdgjh ash") Leaf) (LogMessage Info 5 "sdgjh ash") (Node (Node Leaf (LogMessage Info 6 "sdgjh ash") Leaf) (LogMessage Info 13 "sdgjh ash") Leaf)))

    , testCase "insert error Node"
       ( insert (LogMessage (Error 2) 6 "Completed armadillo processing") (Node (Node Leaf (parseMessage "I 2 sdgjh ash") Leaf) (parseMessage "I 5 sdgjh ash") (Node Leaf  (parseMessage "I 13 sdgjh ash") Leaf ))
        @?= (Node (Node Leaf (LogMessage Info 2 "sdgjh ash") Leaf) (LogMessage Info 5 "sdgjh ash") (Node (Node Leaf (LogMessage (Error 2) 6 "sdgjh ash") Leaf) (LogMessage Info 13 "sdgjh ash") Leaf)))

    , testCase "instance Ord LogMessage Info"
       ( (parseMessage "I 2 sdgjh ash") <= (parseMessage "I 5 sdgjh ash")
        @?= ((LogMessage Info 2 "sdgjh ash") < (LogMessage Info 5 "sdgjh ash")))

    , testCase "instance Ord LogMessage Error"
       ( (parseMessage "E 2 2 sdgjh ash") <= (parseMessage "E 5 5 sdgjh ash")
        @?= ((LogMessage (Error 2) 2 "sdgjh ash") < (LogMessage (Error 5) 5 "sdgjh ash")))

    -- Next week we'll have the computer write more tests, to help us
    -- be more confident that we've tested all the tricky bits and
    -- edge cases.  There are also tools to make sure that our tests
    -- actually run every line of our code (called "coverage"), but we
    -- won't learn those this year.

    -- Write tests for 'inOrder'.  Remember that the input tree is
    -- meant to already be sorted, so it's fine to only test such
    -- inputs.  You may want to reuse MessageTrees from the tests on
    -- 'insert' above.  You may even want to move them elsewhere in
    -- the file and give them names, to more easiely reuse them.

    , testCase "inOrder Empty/Leaf"
       (inOrder (Leaf)
        @?= [])
    , testCase "inOrder List/Node"
           ( inOrder (Node (Node Leaf (LogMessage Info 2 "sdgjh ash") Leaf) (LogMessage Info 5 "sdgjh ash") (Node (Node Leaf (LogMessage (Error 2) 6 "sdgjh ash") Leaf) (LogMessage Info 13 "sdgjh ash") Leaf))
        @?= [LogMessage Info 2 "sdgjh ash" , LogMessage Info 5 "sdgjh ash" , LogMessage Error 2 6 "sdgjh ash" , LogMessage Info 13 "sdgjh ash"])

    , testProperty "build sorted"
    (\msgList -> isSorted (inOrder (build msgList)))

    -- show :: Int -> String
    -- gives the String representation of an Int
    -- Use show to test your code to parse Ints

    -- Write a function that takes a MessageType, and makes a String
    -- with the same format as the log file:
    -- stringMessageType :: MessageType -> String
    -- Use this to test your code that parses MessageType

    -- Make another function that makes a String from a whole LogMessage
    -- stringLogMessage :: LogMessage -> String
    -- Use it to test parseMessage
  ]

main = defaultMain tests
