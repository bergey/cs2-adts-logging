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
  , testCase "parseMessage Warning"
    ( parseMessage "W 4 This is a warning!" @?=
      LogMessage Warning 4 "This is a warning!")

  , testCase "parseMessage Error"
     ( parseMessage "E 4 5 This is an error" @?=
      LogMessage (Error 4) 5 "This is an error")

  , testCase "parseMessage Unknown"
     ( parseMessage "asdf words are written here" @?=
     Unknown "asdf words are written here")
    -- We should also test the smaller parts.  Change the test below
    -- to match the code you actually wrote.
  , testCase "parseType I"
    ( parseType ["I","6","Completed","armadillo","processing"]
      @?=Just Info)

    -- Add at least 3 more tests for MessageType parsing in isolation.
  , testCase "parseType E"
    ( parseType ["E","3","4","tasty","hats","yum"]
    @?= Just (Error 3))

  , testCase "parseType W"
    ( parseType ["W","3","eating","pie","yum"]
    @?= Just Warning)

  , testCase "parseType Unknown"
    ( parseType ["JOIEHR","stuff","kappaPride"]
    @?= Nothing)
    -- Add tests for timestamp parsing.  Think in particular about
    -- what the function does if the input doesn't start with a digit,
    -- or has some spaces followed by digits.
   , testCase "parseTimeStamp oneNum"
    ( parseTimeStamp ["I","6","Completed","armadillo","processing"]
    @?= Just 6)

  , testCase "parseTimeStamp Error"
    ( parseTimeStamp ["E","3","4","tasty","hats","yum"]
    @?= Just 4)

  , testCase "parseTimeStamp NA"
    ( parseTimeStamp ["JOIEHR","stuff","kappaPride"]
    @?= Nothing)
    -- How many tests do you think is enough?  Write at least 3
    -- sentences explaining your decision.

    -- You should have tests for every possible scenario you could have.
    -- Whether that's 100 tests or two, you should try to make sure regardless of
    -- what your code is given it will be able to handle it without dying. That is
    -- what good code does.

    -- Write at least 5 tests for 'insert', with sufficiently
    -- different inputs to test most of the cases.  Look at your code
    -- for 'insert', and any bugs you ran into while writing it.
   , testCase "insertTest 1"
     ( insert (LogMessage Info 4 "yo") Leaf
     @?= Node Leaf (LogMessage Info 4 "yo") Leaf)

   , testCase "insertTest 2"
     ( insert (LogMessage (Error 3) 6 "o boy") (Node Leaf (LogMessage Info 4 "yo") Leaf)
     @?= Node Leaf (LogMessage (Error 3) 6 "o boy") (Node Leaf (LogMessage Info 4 "yo") Leaf))

   , testCase "insertTest 3"
     ( insert (Unknown "string") Leaf
     @?= Leaf)

   , testCase "insertTest 4"
     ( insert (LogMessage Warning 2 "yolo") (Node Leaf (LogMessage Info 4 "yo") Leaf)
     @?= Node (Node Leaf (LogMessage Info 4 "yo") Leaf)(LogMessage Warning 2 "yolo") Leaf)

   , testCase "insertTest 5"
     ( insert (LogMessage Warning 2 "yolo") (Node (Node Leaf (LogMessage Warning 7 "warned") Leaf) (LogMessage Info 4 "yo") Leaf)
     @?= Node (Node (Node Leaf (LogMessage Warning 7 "warned") Leaf) (LogMessage Info 4 "yo") Leaf) (LogMessage Warning 2 "yolo") Leaf)
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
