module Main where

import Test.Validity
import Test.Hspec

import Scheme

main = hspec $
  describe "dummy test" $
    it "always fails" False
