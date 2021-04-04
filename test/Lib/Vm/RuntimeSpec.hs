module Lib.Vm.RuntimeSpec where

import Lib.Vm.Runtime.Context
import Lib.Vm.Runtime.Interpret
import Lib.Vm.Runtime.Structure as RS
import Test.Hspec

spec :: Spec
spec = do
  describe "interpretBinaryOp" $ do
    it "pushes result back on stack" $ do
      let op = InsTBinop $ IntBinaryOp RS.IntBinopAdd
      let intToStackVal i = RS.StackValue $ RS.Number $ RS.IntValue $ RS.I64 i
      let state = InterpretState (Stack [intToStackVal 1, intToStackVal 2]) Nothing
      let res = runContext (interpret op) state
      res `shouldBe` Right ((), InterpretState (Stack [intToStackVal 3]) Nothing)

  describe "numericTypeEquality" $ do
    it "gives exact equality for exact types" $ do
      numericTypeEquality (RS.IntValue $ RS.I32 4) (RS.IntValue $ RS.I32 6) `shouldBe` EqualityExact
