module Main where

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Scheme

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [printParse]

printParse :: TestTree
printParse = testGroup "Parser/Printer Properties" [ppQC]

ppQC = testGroup "QuickChecking Parser/Printer..."
  [ QC.testProperty "Dummy" (\x -> if (x :: Int) < 5 then True else False) ]
