module Lib.Vm.Runtime.Context where

import Control.Monad
import qualified Lib.Vm.Runtime.Structure as RS

newtype Stack a = Stack [a] deriving (Show, Eq)

push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack (x : xs)

pop :: Stack a -> (Maybe a, Stack a)
pop (Stack (x : xs)) = (Just x, Stack xs)
pop (Stack []) = (Nothing, Stack [])

data InterpretState = InterpretState
  { stack :: Stack RS.StackEntry,
    frame :: Maybe RS.Frame, -- TODO: Does a frame always have to exist?
    store :: RS.Store
  }
  deriving (Show, Eq)

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

currentFrame :: InterpretContext RS.Frame
currentFrame = InterpretContext $ \s -> case frame s of
  (Just x) -> Right (x, s)
  _ -> Left "No frame"

setCurrentFrameLocal :: Int -> RS.Value -> InterpretContext ()
setCurrentFrameLocal idx val = InterpretContext $ \s -> case frame s of
  (Just x) -> Right ((), s {frame = Just $ modifyFrame x})
  _ -> Left "No frame"
  where
    modifyFrame f@RS.Frame {RS.frameLocals = l} = f {RS.frameLocals = insertValue l}
    insertValue values = let (xs, ys) = splitAt idx values in xs ++ (val : (tail ys))

getGlobalInstance :: RS.Addr -> InterpretContext RS.GlobalInst
getGlobalInstance addr = InterpretContext $ \s -> Right (globalInst s, s)
  where
    globalInst s = RS.instAtAddr (store s) addr RS.sGlobals

setGlobalInstance :: RS.Addr -> RS.Value -> InterpretContext ()
setGlobalInstance addr val = InterpretContext $ \s -> Right ((), s {store = modifyStore (store s)})
  where
    modifyStore s@RS.Store {RS.sGlobals = globs} = s {RS.sGlobals = insertValue globs}
    insertValue globs =
      let (xs, ys) = splitAt (intAddr addr) globs
       in xs ++ ((replaced globs) {RS.gValue = val} : tail ys)
    replaced globs = globs !! intAddr addr
    intAddr (RS.Addr w) = w

trapError :: InterpretError -> InterpretContext a
trapError e = InterpretContext $ \_ -> Left e

-- | Degree of number type equality.
-- TODO: Needs value for if two numbers are floats (or ints).
data NumericTypeEquality
  = EqualityNumber
  | EqualityExact
  deriving (Show, Eq, Ord)

numericTypeEquality :: RS.NumberValue -> RS.NumberValue -> NumericTypeEquality
numericTypeEquality n1 n2
  | RS.typeEq n1 n2 = EqualityExact
  | otherwise = EqualityNumber

assertNumericTypes :: RS.NumberValue -> RS.NumberValue -> NumericTypeEquality -> InterpretContext ()
assertNumericTypes v1 v2 equality
  | numericTypeEquality v1 v2 >= equality = return ()
  | otherwise = trapError "Numeric type equality outside of bounds"

assertValueTypesEq :: RS.ValueTypeEq a => a -> a -> InterpretContext ()
assertValueTypesEq v1 v2
  | RS.typeEq v1 v2 = return ()
  | otherwise = trapError "Value types not equal"

valueFromStack :: InterpretContext RS.Value
valueFromStack =
  popStack >>= \e -> case e of
    (RS.StackValue v) -> return v
    _ -> trapError "Item on stack not a value"

numberFromStack :: InterpretContext RS.NumberValue
numberFromStack =
  popStack >>= \e -> case e of
    (RS.StackValue (RS.Number v)) -> return v
    _ -> trapError "Item on stack not a number"

refFromStack :: InterpretContext RS.RefValue
refFromStack =
  popStack >>= \e -> case e of
    (RS.StackValue (RS.Ref v)) -> return v
    _ -> trapError "Item on stack not a ref"

pushBool :: Bool -> InterpretContext ()
pushBool True = pushStack $ RS.StackValue $ RS.Number $ RS.IntValue $ RS.I32 1
pushBool False = pushStack $ RS.StackValue $ RS.Number $ RS.IntValue $ RS.I32 0
