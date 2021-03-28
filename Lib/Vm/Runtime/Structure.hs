-- | Types and functions for the runtime structure of the vm.
module Lib.Vm.Runtime.Structure where

import Data.Int
import Data.Word
import qualified Lib.Vm.Structure.Types as S

class ValueTypeEq a where
  typeEq :: a -> a -> Bool

data Value = Number NumberValue | Ref RefValue

instance ValueTypeEq Value where
  typeEq (Number v1) (Number v2) = typeEq v1 v2
  typeEq (Ref v1) (Ref v2) = typeEq v1 v2
  typeEq _ _ = False

data NumberValue
  = U32 Word32
  | U64 Word64
  | I32 Int32
  | I64 Int64
  deriving (Show, Eq)

instance ValueTypeEq NumberValue where
  typeEq (U32 _) (U32 _) = True
  typeEq (U64 _) (U64 _) = True
  typeEq (I32 _) (I32 _) = True
  typeEq (I64 _) (I64 _) = True
  typeEq _ _ = False

newtype Addr = Addr Word64 deriving (Eq)

data RefValue
  = RefNull
  | RefFunc Addr
  | RefExtern Addr
  deriving (Eq)

instance ValueTypeEq RefValue where
  typeEq RefNull RefNull = True
  typeEq (RefFunc _) (RefFunc _) = True
  typeEq (RefExtern _) (RefExtern _) = True
  typeEq _ _ = False

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

data IntUnop = IntUnopClz | IntUnopCtz | IntUnopPopcount

data FloatUnop
  = FloatUnopAbs
  | FloatUnopNeg
  | FloatUnopSqrt
  | FloatUnopCeil
  | FloatUnopFloor
  | FloatUnopTrunc
  | FloatUnopNearest

data UnaryOp = IntUnaryOp IntUnop | FloatUnaryOp FloatUnop

data IntBinop
  = IntBinopAdd
  | IntBinopSub
  | IntBinopMul
  | IntBinopDivU
  | IntBinopDivS
  | IntBinopRemU
  | IntBinopRemS
  | IntBinopAnd
  | IntBinopOr
  | IntBinopXor
  | IntBinopShl
  | IntBinopShrU
  | IntBinopShrS
  | IntBinopRotl
  | IntBinopRotr

data FloatBinop
  = FloatBinopAdd
  | FloatBinopSub
  | FloatBinopMul
  | FloatBinopDiv
  | FloatBinopMin
  | FloatBinopMax
  | FloatBinopCopysign

data BinaryOp = IntBinaryOp IntBinop | FloatBinaryOp FloatBinop

data IntRelop
  = IntRelopEq
  | IntRelopNe
  | IntRelopLtU
  | IntRelopLtS
  | IntRelopGtU
  | IntRelopGtS
  | IntRelopLeU
  | IntRelopLeS
  | IntRelopGeU
  | IntRelopGeS

data FloatRelop
  = FloatRelopEq
  | FloatRelopNe
  | FloatRelopLt
  | FloatRelopGt
  | FloatRelopLe
  | FloatRelopGe

data RelOp = IntRelop IntRelop | FloatRelop FloatRelop

data Instruction
  = InsTConst NumberValue
  | InsTUnop UnaryOp
  | InsTBinop BinaryOp
  | InsTTestop
  | InsTRelop RelOp
  | InsRefNull
  | InsRefIsNull
  | InsDrop
  | InsSelect

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
