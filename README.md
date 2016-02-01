# Log Analysis

The assignment explanation is at:

http://www.cis.upenn.edu/~cis194/spring13/hw/02-ADTs.pdf

# Compiling & Running

Compile your code by running (in Terminal):

`cabal install`

Now you will have an executable file called `LogAnalysis` (note, there is no file extension).  Run your program from Terminal as:

`cs2-adts-logging`

# Uploading to Github

Commit and describe your changes:

`git commit -am 'description goes here'`

Upload to Github:

`git push`

# Testing

I've added tests in Test.hs, and a .cabal file to help install the
libraries needed by the tests, and run the tests.  `cabal` is a build
tool for Haskell - a program that helps us manage libraries and
compile them with our code.  To install the libraries, run:

`cabal install --enable-tests`

To run the tests, run:

`cabal test`
