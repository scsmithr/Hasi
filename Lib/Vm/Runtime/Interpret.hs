module Lib.Vm.Runtime.Interpret where

import Control.Monad
import qualified Lib.Vm.Runtime.Structure as RS

newtype Stack a = Stack [a]

push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack (x : xs)

pop :: Stack a -> (Maybe a, Stack a)
pop (Stack (x : xs)) = (Just x, Stack xs)
pop (Stack []) = (Nothing, Stack [])

data InterpretState = InterpretState
  { stack :: Stack RS.StackEntry
  }

type InterpretError = String

newtype InterpretContext a = InterpretContext
  { runContext :: InterpretState -> Either InterpretError (a, InterpretState)
  }

instance Monad InterpretContext where
  return x = InterpretContext $ \s -> Right (x, s)
  i >>= k = InterpretContext $ \s -> case runContext i s of
    Left e -> Left e
    Right (x, s') -> runContext (k x) s'

instance Functor InterpretContext where
  fmap = liftM

instance Applicative InterpretContext where
  pure = return
  (<*>) = ap

pushStack :: RS.StackEntry -> InterpretContext ()
pushStack entry = InterpretContext $ \s -> Right ((), modifyStack s)
  where
    modifyStack state@InterpretState {stack = s} = state {stack = push entry s}

popStack :: InterpretContext RS.StackEntry
popStack = InterpretContext $ \s -> case pop (stack s) of
  (Just val, stack') -> Right (val, s {stack = stack'})
  _ -> Left "No more values on stack"

trapError :: InterpretError -> InterpretContext ()
trapError e = InterpretContext $ \_ -> Left e

interpret :: RS.Instruction -> InterpretContext ()
interpret (RS.InsTConst val) = pushStack $ RS.StackValue $ RS.Number val
interpret (RS.InsTUnop op) = interpretUnaryOp op
interpret (RS.InsTBinop op) = interpretBinaryOp op
interpret _ = error "unimplemented"

interpretUnaryOp :: RS.UnaryOp -> InterpretContext ()
interpretUnaryOp op = do
  entry <- popStack
  let res = case entry of
        (RS.StackValue (RS.Number v)) -> evalUnaryOp op v
        -- TODO: Function value?
        _ -> Left "Invalid value type on stack"
  case res of
    (Right v) -> pushStack $ RS.StackValue $ RS.Number v
    (Left e) -> trapError e

interpretBinaryOp :: RS.BinaryOp -> InterpretContext ()
interpretBinaryOp op = do
  e1 <- popStack
  e2 <- popStack
  let res = case (e1, e2) of
        (RS.StackValue (RS.Number v1), RS.StackValue (RS.Number v2)) -> evalBinaryOp op v1 v2
        _ -> Left "Invalid value types on stack"
  case res of
    (Right v) -> pushStack $ RS.StackValue $ RS.Number v
    (Left e) -> trapError e

-- TODO: Implement me
evalBinaryOp ::
  RS.BinaryOp ->
  RS.NumberValue ->
  RS.NumberValue ->
  Either InterpretError RS.NumberValue
evalBinaryOp _op _v1 _v2 = Right $ RS.U64 0

-- TODO: Implement me
evalUnaryOp :: RS.UnaryOp -> RS.NumberValue -> Either InterpretError RS.NumberValue
evalUnaryOp (RS.NumericUnaryOp _) _val = Right $ RS.U64 0
evalUnaryOp (RS.FuncUnaryOp RS.FuncUnopAbs) val = Right val
evalUnaryOp (RS.FuncUnaryOp RS.FuncUnopNeg) val = Right val
evalUnaryOp (RS.FuncUnaryOp RS.FuncUnopSqrt) val = Right val
evalUnaryOp (RS.FuncUnaryOp RS.FuncUnopCeil) val = Right val
evalUnaryOp (RS.FuncUnaryOp RS.FuncUnopFloor) val = Right val
evalUnaryOp (RS.FuncUnaryOp RS.FuncUnopTrunc) val = Right val
evalUnaryOp (RS.FuncUnaryOp RS.FuncUnopNearest) val = Right val
