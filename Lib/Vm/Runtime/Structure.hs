-- | Types and functions for the runtime structure of the vm.
module Lib.Vm.Runtime.Structure where

import Data.Int
import Data.Word
import qualified Lib.Vm.Structure.Types as S

data Value = Number NumberValue | Ref RefValue

data NumberValue
  = U32 Word32
  | U64 Word64
  | I32 Int32
  | I64 Int64

newtype Addr = Addr Word64

data RefValue
  = RefNull
  | RefFunc Addr
  | RefExtern Addr

defaultValue :: Value -> Value
defaultValue (Number _) = Number $ U64 0
defaultValue (Ref _) = Ref RefNull

data ModuleInst = ModuleInst
  { mTypes :: [S.FuncType],
    mFuncAddrs :: [Addr],
    mTableAddrs :: [Addr],
    mMemAddrs :: [Addr],
    mGlobalAddrs :: [Addr],
    mElemAddrs :: [Addr],
    mDataAddrs :: [Addr],
    mExports :: [ExportInst]
  }

data ExtValue = ExtFunc Addr | ExtTable Addr | ExtMem Addr | ExtGlobal Addr

data ExportInst = ExportInst {eName :: S.Name, eValue :: ExtValue}

data FuncInst = FuncInst {fInstType :: S.FuncType, fModule :: ModuleInst}

data NumericUnop = NumericUnopClz | NumericUnopCtz | NumericUnopPopcount

data FuncUnop
  = FuncUnopAbs
  | FuncUnopNeg
  | FuncUnopSqrt
  | FuncUnopCeil
  | FuncUnopFloor
  | FuncUnopTrunc
  | FuncUnopNearest

data UnaryOp = NumericUnaryOp NumericUnop | FuncUnaryOp FuncUnop

data NumericBinop
  = NumericBinopAdd
  | NumericBinopSub
  | NumericBinopMul
  | NumericBinopDivU
  | NumericBinopDivS
  | NumericBinopRemU
  | NumericBinopRemS
  | NumericBinopAnd
  | NumericBinopOr
  | NumericBinopXor
  | NumericBinopShl
  | NumericBinopShrU
  | NumericBinopShrS
  | NumericBinopRotl
  | NumericBinopRotr

data FuncBinop
  = FuncBinopAdd
  | FuncBinopSub
  | FuncBinopMul
  | FuncBinopDiv
  | FuncBinopMin
  | FuncBinopMax
  | FuncBinopCopysign

data BinaryOp = NumericBinaryOp NumericBinop | FuncBinaryOp FuncBinop

data Instruction
  = InsTConst NumberValue
  | InsTUnop UnaryOp
  | InsTBinop BinaryOp

data Label = Label
  { labelArity :: Int,
    labelTarget :: [Instruction]
  }

data Frame = Frame
  { frameLocals :: [Value],
    frameModule :: ModuleInst
  }

data Activation = Activation
  { activationFrameArity :: Int,
    activationFrame :: Frame
  }

data StackEntry
  = StackValue Value
  | StackLabel Label
  | StackActivation Activation
  | StackFrame Frame
