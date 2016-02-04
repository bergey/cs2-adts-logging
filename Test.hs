-- This is how we import only a few definitions from Test.Tasty
import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit

import Log

-- import everything *except* `main` from LogAnalysis
import LogAnalysis hiding (main)

l1 = LogMessage Info 1 "hi1"
l2 = LogMessage Warning 12 "hi2"
l3 = LogMessage Warning 13 "hi3"
l4 = LogMessage Info 14 "hi4"
l5 = LogMessage Warning 15 "hi5"
l6 = LogMessage Info 16 "hi6"
l7 = LogMessage Warning 17 "hi7"
l8 = LogMessage Info 18 "hi8"
l9 = LogMessage Info 19 "hi9"

tests :: TestTree
tests = testGroup "unit tests"
  [ testCase "parseMessage Info"
    ( parseMessage "I 6 Completed armadillo processing" @?=
      LogMessage Info 6 "Completed armadillo processing" )
    -- If you don't have a function called praseMessage, change the
    -- test to match your code.
    
  --  -- Add at least 3 more test cases for 'parseMessage', including
  --  -- one with an error, one with a warning, and one with an Unknown
    ,testCase "parseMessage Error"
    ( parseMessage "E 90 12 Armadillo Escaped the cage!" @?=
        LogMessage (Error 90) 12 "Armadillo Escaped the cage!" )

    ,testCase "parseMessage Warning"
    ( parseMessage "W 10 The cage door is open" @?=
        LogMessage Warning 10 "The cage door is open" )

    ,testCase "parseMessage Unknown"
    ( parseMessage "This should not be working" @?=
        Unknown "This should not be working" )

  --  -- We should also test the smaller parts.  Change the test below
  --  -- to match the code you actually wrote.
  , testCase "checkMessageType I"
    ( checkMessageType 'I'
      @?= Just Info)


  , testCase "checkMessageType E"
    ( checkMessageType 'E'
      @?= Just (Error 0))

  , testCase "checkMessageType W"
    ( checkMessageType 'W'
        @?= Just Warning)
    
  , testCase "checkMessageType Unknown"
    ( checkMessageType 'C'
        @?= Nothing)

  --  -- Add at least 3 more tests for MessageType parsing in isolation.

  --  -- Add tests for timestamp parsing.  Think in particular about
  --  -- what the function does if the input doesn't start with a digit,
  --  -- or has some spaces followed by digits.

  , testCase "checkTimeStamp Single Diget"
    ( checkTimeStamp (0, "6 Completed armadillo processing")
        @?= (0, (6,"Completed armadillo processing")))

  , testCase "checkTimeStamp Double Diget"
    ( checkTimeStamp (0, "66 Completed armadillo processing")
        @?= (0, (66,"Completed armadillo processing")))

  , testCase "checkTimeStamp Triple Diget"
    ( checkTimeStamp (0, "666 Completed armadillo processing")
        @?= (0, (666,"Completed armadillo processing")))

  , testCase "checkTimeStamp Quadruple Diget"
    ( checkTimeStamp (0, "6666 Completed armadillo processing")
        @?= (0, (6666,"Completed armadillo processing")))
    -- How many tests do you think is enough?  Write at least 3
    -- sentences explaining your decision.

--I think it is enough tests when you can tell based off of which test fails, which part is failing. For instance, if the error fails, I know that
--the program is failing in recognizing errors. Or for Insert, that it failed because I was missing a blank case. As long as from my tests, I can tell which program is failing,
--on which acceptable imput, I feel that is sufficient tests. 

    -- Write at least 5 tests for 'insert', with sufficiently
    -- different inputs to test most of the cases.  Look at your code
    -- for 'insert', and any bugs you ran into while writing it.

  , testCase "insert blank"
    ( insert (LogMessage Info 6 "Completed armadillo processing") Leaf
        @?= Node Leaf (LogMessage Info 6 "Completed armadillo processing") Leaf)

  , testCase "insert moreNew"
    ( insert (LogMessage Info 15 "Completed armadillo processing") (Node Leaf (LogMessage Warning 10 "The cage door is open") Leaf)
        @?= Node Leaf (LogMessage Warning 10 "The cage door is open") (Node Leaf (LogMessage Info 15 "Completed armadillo processing") Leaf))

  , testCase "insert lessNew"
    ( insert (LogMessage Info 6 "Completed armadillo processing") (Node Leaf (LogMessage Warning 10 "The cage door is open") Leaf)
        @?= Node (Node Leaf (LogMessage Info 6 "Completed armadillo processing") Leaf) (LogMessage Warning 10 "The cage door is open") Leaf)

  , testCase "insert lessNewDouble"
    ( insert (LogMessage Info 6 "hi") (Node (Node Leaf (LogMessage Info 15 "sup") Leaf) (LogMessage Warning 50 "bye") Leaf)
        @?= Node (Node (Node Leaf (LogMessage Info 6 "hi") Leaf) (LogMessage Info 15 "sup") Leaf) (LogMessage Warning 50 "bye") Leaf)

  , testCase "insert moreNewDouble"
    ( insert (LogMessage Info 100 "hi") (Node Leaf (LogMessage Warning 20 "bye") (Node Leaf (LogMessage Info 50 "sup") Leaf))
        @?= Node Leaf (LogMessage Warning 20 "bye") (Node Leaf (LogMessage Info 50 "sup") (Node Leaf (LogMessage Info 100 "hi") Leaf)))

    -- Next week we'll have the computer write more tests, to help us
    -- be more confident that we've tested all the tricky bits and
    -- edge cases.  There are also tools to make sure that our tests
    -- actually run every line of our code (called "coverage"), but we
    -- won't learn those this year.

  , testCase "inOrder full"
    ( inOrder (Node (Node (Node Leaf l1 Leaf) l2 (Node (Node Leaf l3 Leaf) l4 (Node Leaf l5 Leaf))) l6 (Node Leaf l7 (Node Leaf l8 (Node Leaf l9 Leaf))))
        @?= [l1,l2,l3,l4,l5,l6,l7,l8,l9] )

  , testCase "inOrder empty"
    ( inOrder Leaf
        @?= [] )

    -- Write tests for 'inOrder'.  Remember that the input tree is
    -- meant to already be sorted, so it's fine to only test such
    -- inputs.  You may want to reuse MessageTrees from the tests on
    -- 'insert' above.  You may even want to move them elsewhere in
    -- the file and give them names, to more easiely reuse them.
  
  ]

main = defaultMain tests
