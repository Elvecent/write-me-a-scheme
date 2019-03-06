module Main where

import Test.Tasty
import Test.Tasty.QuickCheck as QC

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [parserProps]

parserProps :: TestTree
parserProps = testGroup "Parser/Printer Properties" [printParse]

printParse = testGroup "QuickChecking..."
  [ QC.testProperty "Dummy" (\x -> if (x :: Int) < 5 then True else False) ]
