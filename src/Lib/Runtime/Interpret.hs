{-# LANGUAGE FlexibleContexts #-}

module Lib.Runtime.Interpret (interpret) where

import Control.Monad
import Data.Int
import Data.List
import Lib.Runtime.Context
import qualified Lib.Runtime.Structure as RS

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
interpret (RS.InsTableSize idx) = interpretTableSize idx
interpret (RS.InsTableGrow idx) = interpretTableGrow idx
interpret (RS.InsTableFill idx) = interpretTableFill idx
interpret (RS.InsTableCopy idxX idxY) = interpretTableCopy idxX idxY
interpret (RS.InsTableInit idxX idxY) = interpretTableInit idxX idxY
interpret (RS.InsElemDrop idx) = interpretElemDrop idx
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
  c <- popUnwrapI32
  v2 <- valueFromStack
  v1 <- valueFromStack
  _ <- assertValueTypesEq v1 v2
  case c of
    0 -> pushStack $ RS.StackValue v1
    _ -> pushStack $ RS.StackValue v2

interpretLocalGet :: Int -> InterpretContext ()
interpretLocalGet idx = do
  f <- currentFrame
  let local = RS.frameLocals f !! idx -- TODO: Assert index exists.
  pushStack $ RS.StackValue local

interpretLocalSet :: Int -> InterpretContext ()
interpretLocalSet idx = do
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
  addr <- addrFromFrameModule ((!! idx) . RS.mGlobalAddrs)
  glob <- getGlobalInstance addr
  let val = RS.gValue glob
  pushStack $ RS.StackValue val

interpretGlobalSet :: Int -> InterpretContext ()
interpretGlobalSet idx = do
  addr <- addrFromFrameModule ((!! idx) . RS.mGlobalAddrs)
  val <- valueFromStack
  setGlobalInstance addr val

interpretTableGet :: Int -> InterpretContext ()
interpretTableGet idx = do
  (_, tab) <- tableAddrInstPair idx
  i <- popUnwrapI32
  let ii = fromIntegral i :: Int
  case ii < length (RS.tElem tab) of
    True -> pushStack $ RS.StackValue $ RS.Ref $ RS.tElem tab !! ii
    False -> trapError "Index out of bounds"

interpretTableSet :: Int -> InterpretContext ()
interpretTableSet idx = do
  (addr, tab) <- tableAddrInstPair idx
  r <- refFromStack
  i <- popUnwrapI32
  let ii = fromIntegral i :: Int
  case ii < length (RS.tElem tab) of
    True ->
      let updated = tab {RS.tElem = replaceValue ii r (RS.tElem tab)}
       in setTable addr updated
    False -> trapError "Index out of bounds"

interpretTableSize :: Int -> InterpretContext ()
interpretTableSize idx = do
  (_, tab) <- tableAddrInstPair idx
  let sz = length (RS.tElem tab)
  pushI32 sz

interpretTableGrow :: Int -> InterpretContext ()
interpretTableGrow idx = do
  (addr, tab) <- tableAddrInstPair idx
  let sz = length (RS.tElem tab)
  n <- popUnwrapI32
  r <- refFromStack
  let elems = RS.tElem tab ++ genericReplicate n r
  let updated = tab {RS.tElem = elems}
  _ <- setTable addr updated
  pushI32 sz

interpretTableFill :: Int -> InterpretContext ()
interpretTableFill idx = do
  (_, tab) <- tableAddrInstPair idx
  n <- popUnwrapI32
  val <- refFromStack
  i <- popUnwrapI32
  let run
        | i + n > genericLength (RS.tElem tab) = trapError "Index out of bounds"
        | n == 0 = return ()
        | otherwise =
          pushI32 i
            >> pushRef val
            >> interpret (RS.InsTableSet idx)
            >> pushI32 (i + 1)
            >> pushRef val
            >> pushI32 (n -1)
            >> interpret (RS.InsTableFill idx)
  run

interpretTableCopy :: Int -> Int -> InterpretContext ()
interpretTableCopy idxX idxY = do
  (_, tabX) <- tableAddrInstPair idxX
  (_, tabY) <- tableAddrInstPair idxY
  n <- popUnwrapI32
  s <- popUnwrapI32
  d <- popUnwrapI32
  let postRun :: InterpretContext ()
      postRun = pushI32 (n -1) >> interpret (RS.InsTableCopy idxX idxY)

      run :: InterpretContext ()
      run
        | s + n > genericLength (RS.tElem tabY) || d + n > genericLength (RS.tElem tabX) =
          trapError "Index out of bounds"
        | n == 0 = return ()
        | d <= s =
          pushI32 d
            >> pushI32 s
            >> interpret (RS.InsTableGet idxY)
            >> interpret (RS.InsTableSet idxX)
            >> pushI32 (d + 1)
            >> pushI32 (s + 1)
            >> postRun
        | otherwise =
          pushI32 (d + n -1)
            >> pushI32 (s + n -1)
            >> interpret (RS.InsTableGet idxY)
            >> interpret (RS.InsTableSet idxX)
            >> pushI32 d
            >> pushI32 s
            >> postRun
  run

interpretTableInit :: Int -> Int -> InterpretContext ()
interpretTableInit idxX idxY = do
  (_, tabX) <- tableAddrInstPair idxX
  (_, elemY) <- elemAddrInstPair idxY
  n <- popUnwrapI32
  s <- popUnwrapI32
  d <- popUnwrapI32
  let run :: InterpretContext ()
      run
        | s + n > genericLength (RS.eElem elemY) || d + n > genericLength (RS.tElem tabX) =
          trapError "Index out of bounds"
        | n == 0 = return ()
        | otherwise =
          let val = RS.eElem elemY !! fromIntegral s
           in pushI32 d
                >> pushRef val
                >> interpret (RS.InsTableSet idxX)
                >> pushI32 (d + 1)
                >> pushI32 (s + 1)
                >> pushI32 (n -1)
                >> interpret (RS.InsTableInit idxX idxY)
  run

interpretElemDrop :: Int -> InterpretContext ()
interpretElemDrop idx = do
  (addr, e) <- elemAddrInstPair idx
  let updated = e {RS.eElem = []}
  setElemInstance addr updated

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

pushBool :: Bool -> InterpretContext ()
pushBool True = pushStack $ RS.StackValue $ RS.Number $ RS.IntValue $ RS.I32 1
pushBool False = pushStack $ RS.StackValue $ RS.Number $ RS.IntValue $ RS.I32 0

pushI32 :: Integral a => a -> InterpretContext ()
pushI32 = pushStack . RS.StackValue . RS.Number . RS.IntValue . RS.I32 . fromIntegral

popUnwrapI32 :: InterpretContext Int32
popUnwrapI32 = numberFromStack >>= liftEither . unwrapI32

pushRef :: RS.RefValue -> InterpretContext ()
pushRef = pushStack . RS.StackValue . RS.Ref

-- | Get the table address and instance pair from the current frame's module at
-- the given index.
tableAddrInstPair :: Int -> InterpretContext (RS.Addr, RS.TableInst)
tableAddrInstPair idx = do
  addr <- addrFromFrameModule ((!! idx) . RS.mTableAddrs)
  tab <- getTableInstance addr
  return (addr, tab)

-- | Get the element address and instance from the current frame's module.
elemAddrInstPair :: Int -> InterpretContext (RS.Addr, RS.ElemInst)
elemAddrInstPair idx = do
  addr <- addrFromFrameModule ((!! idx) . RS.mElemAddrs)
  e <- getElemInstance addr
  return (addr, e)
