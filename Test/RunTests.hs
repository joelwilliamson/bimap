#!/usr/bin/env runhaskell
{-# LANGUAGE TemplateHaskell #-}

{-
A stub file that uses Test.Util to extract and splice all the
test names from Test.Tests.
-}

import Test.Tests
import Test.Util
import System.Exit

main :: IO ()
main = do
  allPassed <- $( extractTests "Test/Tests.hs" )
  if allPassed
    then exitSuccess
    else exitFailure
