module Lib.Vm.RuntimeSpec where

import Lib.Vm.Runtime.Context
import Lib.Vm.Runtime.Structure as RS
import Test.Hspec

spec :: Spec
spec = do
  describe "numericTypeEquality" $ do
    it "gives exact equality for exact types" $ do
      numericTypeEquality (RS.I32 4) (RS.I32 6) `shouldBe` EqualityExact
