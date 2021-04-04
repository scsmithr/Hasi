module Lib.Vm.Runtime.Interpret (interpret) where

import Control.Monad
import Lib.Vm.Runtime.Context
import qualified Lib.Vm.Runtime.Structure as RS

i32sentinel :: RS.NumberValue
i32sentinel = RS.IntValue $ RS.I32 0

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

-- TODO: Float test op?
interpretTestOp :: InterpretContext ()
interpretTestOp = numberFromStack >>= pushStack . testValue
  where
    testValue (RS.IntValue (RS.U32 0)) = RS.StackValue $ RS.Number $ RS.IntValue $ RS.I32 1
    testValue (RS.IntValue (RS.U64 0)) = RS.StackValue $ RS.Number $ RS.IntValue $ RS.I32 1
    testValue (RS.IntValue (RS.I32 0)) = RS.StackValue $ RS.Number $ RS.IntValue $ RS.I32 1
    testValue (RS.IntValue (RS.I64 0)) = RS.StackValue $ RS.Number $ RS.IntValue $ RS.I32 1
    testValue _ = RS.StackValue $ RS.Number $ RS.IntValue $ RS.I32 0

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
    (RS.IntValue (RS.I32 0)) -> pushStack $ RS.StackValue v1
    (RS.IntValue (RS.I32 _)) -> pushStack $ RS.StackValue v2
    _ -> trapError "Asserted i32, but wasn't i32" -- Shouldn't happen

-- TODO: Implement me
evalBinaryOp ::
  RS.BinaryOp ->
  RS.NumberValue ->
  RS.NumberValue ->
  Either InterpretError RS.NumberValue
evalBinaryOp instr (RS.IntValue v1) (RS.IntValue v2) = RS.IntValue <$> evalIntBinaryOp instr v1 v2
evalBinaryOp _op _v1 _v2 = Right $ RS.IntValue $ RS.U64 0

evalIntBinaryOp ::
  RS.BinaryOp ->
  RS.IntValue ->
  RS.IntValue ->
  Either InterpretError RS.IntValue
evalIntBinaryOp instr (RS.U32 n1) (RS.U32 n2) = binIntOp instr >>= \op -> Right $ RS.U32 $ op n1 n2
evalIntBinaryOp instr (RS.U64 n1) (RS.U64 n2) = binIntOp instr >>= \op -> Right $ RS.U64 $ op n1 n2
evalIntBinaryOp instr (RS.I32 n1) (RS.I32 n2) = binIntOp instr >>= \op -> Right $ RS.I32 $ op n1 n2
evalIntBinaryOp instr (RS.I64 n1) (RS.I64 n2) = binIntOp instr >>= \op -> Right $ RS.I64 $ op n1 n2
evalIntBinaryOp _ _ _ = Left "Unmatched op"

binIntOp :: Num a => RS.BinaryOp -> Either InterpretError (a -> a -> a)
binIntOp (RS.IntBinaryOp RS.IntBinopAdd) = Right (+)
binIntOp _ = Left "Missing op"

-- TODO: Implement me
evalUnaryOp :: RS.UnaryOp -> RS.NumberValue -> Either InterpretError RS.NumberValue
evalUnaryOp _op _val = Right $ RS.IntValue $ RS.U64 0

-- TODO: Implement me
evalRelOp ::
  RS.RelOp ->
  RS.NumberValue ->
  RS.NumberValue ->
  Either InterpretError RS.NumberValue
evalRelOp _op _v1 _v2 = Right $ RS.IntValue $ RS.I32 0
