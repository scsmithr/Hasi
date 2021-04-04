{-# LANGUAGE FlexibleContexts #-}

module Lib.Vm.Runtime.Interpret (interpret) where

import Control.Monad
import Data.Int
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
interpret RS.InsSelect = interpretSelect
interpret (RS.InsLocalGet idx) = interpretLocalGet idx
interpret (RS.InsLocalSet idx) = interpretLocalSet idx
interpret (RS.InsLocalTee idx) = interpretLocalTee idx
interpret (RS.InsGlobalGet idx) = interpretGlobalGet idx
interpret (RS.InsGlobalSet idx) = interpretGlobalSet idx
interpret (RS.InsTableGet idx) = interpretTableGet idx
interpret (RS.InsTableSet idx) = interpretTableSet idx
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

interpretLocalGet :: Int -> InterpretContext ()
interpretLocalGet idx = do
  f <- currentFrame
  let local = RS.frameLocals f !! idx -- TODO: Assert index exists.
  pushStack $ RS.StackValue local

interpretLocalSet :: Int -> InterpretContext ()
interpretLocalSet idx = do
  _ <- currentFrame -- TODO: Should probably remove if frame is always required.
  val <- valueFromStack
  setCurrentFrameLocal idx val

interpretLocalTee :: Int -> InterpretContext ()
interpretLocalTee idx = do
  val <- valueFromStack
  _ <- pushStack $ RS.StackValue val
  _ <- pushStack $ RS.StackValue val
  interpret (RS.InsLocalSet idx)

interpretGlobalGet :: Int -> InterpretContext ()
interpretGlobalGet idx = do
  f <- currentFrame
  addr <- addrFromFrameModule ((!! idx) . RS.mGlobalAddrs)
  glob <- getGlobalInstance addr
  let val = RS.gValue glob
  pushStack $ RS.StackValue val

interpretGlobalSet :: Int -> InterpretContext ()
interpretGlobalSet idx = do
  f <- currentFrame
  addr <- addrFromFrameModule ((!! idx) . RS.mGlobalAddrs)
  val <- valueFromStack
  setGlobalInstance addr val

interpretTableGet :: Int -> InterpretContext ()
interpretTableGet idx = do
  addr <- addrFromFrameModule ((!! idx) . RS.mTableAddrs)
  tab <- getTableInstance addr
  i <- numberFromStack >>= liftEither . unwrapI32
  let ii = fromIntegral i :: Int
  case ii < length (RS.tElem tab) of
    True -> pushStack $ RS.StackValue $ RS.Ref $ RS.tElem tab !! ii
    False -> trapError "Index out of bounds"

interpretTableSet :: Int -> InterpretContext ()
interpretTableSet idx = do
  addr <- addrFromFrameModule ((!! idx) . RS.mTableAddrs)
  tab <- getTableInstance addr
  r <- refFromStack
  i <- numberFromStack >>= liftEither . unwrapI32
  let ii = fromIntegral i :: Int
  case ii < length (RS.tElem tab) of
    True -> setTableElem addr ii r
    False -> trapError "Index out of bounds"

interpretTableFill :: Int -> InterpretContext ()
interpretTableFill idx = do
  addr <- addrFromFrameModule ((!! idx) . RS.mTableAddrs)
  tab <- getTableInstance addr
  n <- numberFromStack
  _ <- assertNumericTypes n i32sentinel EqualityExact
  val <- refFromStack
  i <- numberFromStack
  _ <- assertNumericTypes i i32sentinel EqualityExact
  return () -- TODO

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

unwrapI32 :: RS.NumberValue -> Either InterpretError Int32
unwrapI32 (RS.IntValue (RS.I32 v)) = Right v
unwrapI32 _ = Left "Number value not an i32"
