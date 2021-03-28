module Lib.Vm.Runtime.Interpret where

import Control.Monad
import Lib.Vm.Runtime.Context
import qualified Lib.Vm.Runtime.Structure as RS

i32sentinel :: RS.NumberValue
i32sentinel = RS.I32 0

interpret :: RS.Instruction -> InterpretContext ()
interpret (RS.InsTConst val) = pushStack $ RS.StackValue $ RS.Number val
interpret (RS.InsTUnop op) = interpretUnaryOp op
interpret (RS.InsTBinop op) = interpretBinaryOp op
interpret RS.InsTTestop = interpretTestOp
interpret (RS.InsTRelop op) = interpretRelOp op
interpret RS.InsRefNull = pushStack $ RS.StackValue $ RS.Ref RS.RefNull
interpret RS.InsRefIsNull = refFromStack >>= pushBool . (RS.RefNull ==)
interpret RS.InsDrop = void valueFromStack
interpret _ = error "unimplemented"

interpretUnaryOp :: RS.UnaryOp -> InterpretContext ()
interpretUnaryOp op = do
  v <- numberFromStack
  let res = evalUnaryOp op v
  case res of
    (Right r) -> pushStack $ RS.StackValue $ RS.Number r
    (Left e) -> trapError e

interpretBinaryOp :: RS.BinaryOp -> InterpretContext ()
interpretBinaryOp op = do
  v2 <- numberFromStack
  v1 <- numberFromStack
  _ <- assertNumericTypes v1 v2 EqualityExact
  let res = evalBinaryOp op v1 v2
  case res of
    (Right v) -> pushStack $ RS.StackValue $ RS.Number v
    (Left e) -> trapError e

interpretTestOp :: InterpretContext ()
interpretTestOp = numberFromStack >>= pushStack . testValue
  where
    testValue (RS.U32 0) = RS.StackValue $ RS.Number $ RS.I32 1
    testValue (RS.U64 0) = RS.StackValue $ RS.Number $ RS.I32 1
    testValue (RS.I32 0) = RS.StackValue $ RS.Number $ RS.I32 1
    testValue (RS.I64 0) = RS.StackValue $ RS.Number $ RS.I32 1
    testValue _ = RS.StackValue $ RS.Number $ RS.I32 0

interpretRelOp :: RS.RelOp -> InterpretContext ()
interpretRelOp op = do
  v2 <- numberFromStack
  v1 <- numberFromStack
  _ <- assertNumericTypes v1 v2 EqualityExact
  let res = evalRelOp op v1 v2
  case res of
    (Right r) -> pushStack $ RS.StackValue $ RS.Number r
    (Left e) -> trapError e

interpretSelect :: InterpretContext ()
interpretSelect = do
  c <- numberFromStack
  _ <- assertNumericTypes c i32sentinel EqualityExact
  v2 <- valueFromStack
  v1 <- valueFromStack
  _ <- assertValueTypesEq v1 v2
  case c of
    (RS.I32 0) -> pushStack $ RS.StackValue v2
    (RS.I32 _) -> pushStack $ RS.StackValue v2
    otherwise -> trapError "Asserted i32, but wasn't i32" -- Shouldn't happen

-- TODO: Implement me
evalBinaryOp ::
  RS.BinaryOp ->
  RS.NumberValue ->
  RS.NumberValue ->
  Either InterpretError RS.NumberValue
evalBinaryOp _op _v1 _v2 = Right $ RS.U64 0

-- TODO: Implement me
evalUnaryOp :: RS.UnaryOp -> RS.NumberValue -> Either InterpretError RS.NumberValue
evalUnaryOp _op _val = Right $ RS.U64 0

-- TODO: Implement me
evalRelOp ::
  RS.RelOp ->
  RS.NumberValue ->
  RS.NumberValue ->
  Either InterpretError RS.NumberValue
evalRelOp _op _v1 _v2 = Right $ RS.I32 0
