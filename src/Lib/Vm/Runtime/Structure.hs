-- | Types and functions for the runtime structure of the vm.
module Lib.Vm.Runtime.Structure where

import Data.Int
import Data.Word
import qualified Lib.Vm.Structure.Types as S

class ValueTypeEq a where
  typeEq :: a -> a -> Bool

data Value = Number NumberValue | Ref RefValue deriving (Show, Eq)

instance ValueTypeEq Value where
  typeEq (Number v1) (Number v2) = typeEq v1 v2
  typeEq (Ref v1) (Ref v2) = typeEq v1 v2
  typeEq _ _ = False

data NumberValue = FloatValue FloatValue | IntValue IntValue deriving (Show, Eq)

data IntValue
  = U32 Word32
  | U64 Word64
  | I32 Int32
  | I64 Int64
  deriving (Show, Eq)

data FloatValue
  = F32 Double
  | F64 Double
  deriving (Show, Eq)

instance ValueTypeEq NumberValue where
  typeEq (IntValue (U32 _)) (IntValue (U32 _)) = True
  typeEq (IntValue (U64 _)) (IntValue (U64 _)) = True
  typeEq (IntValue (I32 _)) (IntValue (I32 _)) = True
  typeEq (IntValue (I64 _)) (IntValue (I64 _)) = True
  typeEq (FloatValue (F32 _)) (FloatValue (F32 _)) = True
  typeEq (FloatValue (F64 _)) (FloatValue (F64 _)) = True
  typeEq _ _ = False

newtype Addr = Addr Word64 deriving (Eq, Show)

data RefValue
  = RefNull
  | RefFunc Addr
  | RefExtern Addr
  deriving (Eq, Show)

instance ValueTypeEq RefValue where
  typeEq RefNull RefNull = True
  typeEq (RefFunc _) (RefFunc _) = True
  typeEq (RefExtern _) (RefExtern _) = True
  typeEq _ _ = False

defaultValue :: Value -> Value
defaultValue (Number _) = Number $ IntValue $ U64 0
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
  deriving (Show, Eq)

data ExtValue = ExtFunc Addr | ExtTable Addr | ExtMem Addr | ExtGlobal Addr deriving (Show, Eq)

data ExportInst = ExportInst {eName :: S.Name, eValue :: ExtValue} deriving (Show, Eq)

data FuncInst = FuncInst {fInstType :: S.FuncType, fModule :: ModuleInst} deriving (Show, Eq)

data IntUnop = IntUnopClz | IntUnopCtz | IntUnopPopcount deriving (Show, Eq)

data FloatUnop
  = FloatUnopAbs
  | FloatUnopNeg
  | FloatUnopSqrt
  | FloatUnopCeil
  | FloatUnopFloor
  | FloatUnopTrunc
  | FloatUnopNearest
  deriving (Show, Eq)

data UnaryOp = IntUnaryOp IntUnop | FloatUnaryOp FloatUnop deriving (Show, Eq)

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
  deriving (Show, Eq)

data FloatBinop
  = FloatBinopAdd
  | FloatBinopSub
  | FloatBinopMul
  | FloatBinopDiv
  | FloatBinopMin
  | FloatBinopMax
  | FloatBinopCopysign
  deriving (Show, Eq)

data BinaryOp = IntBinaryOp IntBinop | FloatBinaryOp FloatBinop deriving (Show, Eq)

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
  deriving (Show, Eq)

data FloatRelop
  = FloatRelopEq
  | FloatRelopNe
  | FloatRelopLt
  | FloatRelopGt
  | FloatRelopLe
  | FloatRelopGe
  deriving (Show, Eq)

data RelOp = IntRelop IntRelop | FloatRelop FloatRelop deriving (Show, Eq)

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
  deriving (Show, Eq)

data Label = Label
  { labelArity :: Int,
    labelTarget :: [Instruction]
  }
  deriving (Show, Eq)

data Frame = Frame
  { frameLocals :: [Value],
    frameModule :: ModuleInst
  }
  deriving (Show, Eq)

data Activation = Activation
  { activationFrameArity :: Int,
    activationFrame :: Frame
  }
  deriving (Show, Eq)

data StackEntry
  = StackValue Value
  | StackLabel Label
  | StackActivation Activation
  deriving (Show, Eq)
