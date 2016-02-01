-- This is how we import only a few definitions from Test.Tasty
import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit

import Log

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
    ,testCase "parseMessage Error"
    ( parseMessage "E 90 12 Armadillo Escaped the cage!" @?=
        LogMessage (Error 90) 12 "Armadillo Escaped the cage!" )

    ,testCase "parseMessage Warning"
    ( parseMessage "W 10 The cage door is open" @?=
        LogMessage Warning 10 "The cage door is open" )

    ,testCase "parseMessage Unknown"
    ( parseMessage "This should not be working" @?=
        Unknown "This should not be working" )

    -- We should also test the smaller parts.  Change the test below
    -- to match the code you actually wrote.
  , testCase "checkMessageType I"
    ( checkMessageType "I 6 Completed armadillo processing"
      @?= Just Info)

  , testCase "checkMessageType E"
    ( checkMessageType "E 90 12 Armadillo Escaped the cage!"
      @?= Just Error)

  , testCase "checkMessageType W"
    ( checkMessageType "W 10 The cage door is open"
        @?= Just Warning)
    
  , testCase "checkMessageType Unknown"
    ( checkMessageType "This should not be working"
        @? Nothing)

    -- Add at least 3 more tests for MessageType parsing in isolation.

    -- Add tests for timestamp parsing.  Think in particular about
    -- what the function does if the input doesn't start with a digit,
    -- or has some spaces followed by digits.

  , testCase "checkTimeStamp Message"
    ( checkTimeStamp (0, "6 Completed armadillo processing")
        @? (0, (6,"Completed armadillo processing")))



    -- How many tests do you think is enough?  Write at least 3
    -- sentences explaining your decision.

    -- Write at least 5 tests for 'insert', with sufficiently
    -- different inputs to test most of the cases.  Look at your code
    -- for 'insert', and any bugs you ran into while writing it.

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

  ]

main = defaultMain tests
