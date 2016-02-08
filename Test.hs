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
    ( parseMessage "W 6 the armadillo is processing" @?=
      LogMessage Warning 6 "the armadillo is processing" )

  ,  testCase "parseMessage Error Int"
    ( parseMessage "E 45 6 armadillos can't process" @?=
      LogMessage (Error 45) 6 "armadillos can't process" )

    -- We should also test the smaller parts.  Change the test below
    -- to match the code you actually wrote.
  

  , testCase "Data type Info test for checkmessagemessagetype"
    ( checkmessagemessagetype ["I","help","uoy","mum m8"]
  
      @?= "help")



  , testCase "Data type Warning test for checkmessagemessagetype"
    ( checkmessagemessagetype ["W","help","uoy","mum m8"]
      @?= "help")



  , testCase "Data type Error test for checkmessagemessagetype"
    ( checkmessagemessagetype ["E","2","uoy","mum m8"]
      @?= "uoy")
  
    -- Add at least 3 more tests for MessageType parsing in isolation.

    -- Add tests for timestamp parsing.  Think in particular about
    -- what the function does if the input doesn't start with a digit,
    -- or has some spaces followed by digits.
    
    --Test the time stamp contions

    , testCase "dropthisamount testing otherwise"
    ( dropthisamount ["I","2","uoy","mum m8"]
      @?= 2)

    , testCase "dropthisamount testing Error b output"
    ( dropthisamount ["E","2","uoy","mum m8"]
      @?= 3)


    -- How many tests do you think is enough?  Write at least 3
    -- sentences explaining your decision.

--I think you need to test every function with atleast every way it could ever be used in the program
--What I mean by this is if a function has three uses in a program or rather three possible outputs
-- Then you need three test


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
