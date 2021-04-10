-- | Types and functions for the runtime structure of the vm.
module Lib.Runtime.Structure where

import qualified Data.ByteString.Lazy as B
import Data.Int
import Data.Word
import qualified Lib.Structure as S

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
  = F32 Float
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

newtype Addr = Addr Int deriving (Eq, Show)

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

data TableInst = TableInst {tInstType :: S.TableType, tElem :: [RefValue]} deriving (Show, Eq)

data MemInst = MemInst {mInstType :: S.MemType, mBytes :: B.ByteString} deriving (Show, Eq)

data GlobalInst = GlobalInst {gInstType :: S.ValueType, gValue :: Value} deriving (Show, Eq)

data ElemInst = ElemInst {eInstType :: S.RefType, eElem :: [RefValue]} deriving (Show, Eq)

newtype DataInst = DataInst {dData :: B.ByteString} deriving (Show, Eq)

data Store = Store
  { sFuncs :: [FuncInst],
    sTables :: [TableInst],
    sMems :: [MemInst],
    sGlobals :: [GlobalInst],
    sElems :: [ElemInst],
    sDatas :: [DataInst]
  }
  deriving (Show, Eq)

instAtAddr :: Store -> Addr -> (Store -> [a]) -> a
instAtAddr store (Addr w) proj = proj store !! w

emptyStore :: Store
emptyStore = Store [] [] [] [] [] []

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

data MemArg = MemArg {maOffset :: Word32, maAlign :: Word32} deriving (Show, Eq)

data IntStoreSize = IntStoreSize8 | IntStoreSize16 | IntStoreSize32 deriving (Show, Eq)

storeSize :: IntStoreSize -> Int
storeSize IntStoreSize8 = 8
storeSize IntStoreSize16 = 16
storeSize IntStoreSize32 = 32

data IntSign = IntSignU | IntSignS deriving (Show, Eq)

data NumberType
  = NumberTypeI32
  | NumberTypeI64
  | NumberTypeU32
  | NumberTypeU64
  | NumberTypeF32
  | NumberTypeF64
  deriving (Show, Eq)

bitWidth :: NumberType -> Int
bitWidth NumberTypeI32 = 32
bitWidth NumberTypeI64 = 64
bitWidth NumberTypeU32 = 32
bitWidth NumberTypeU64 = 64
bitWidth NumberTypeF32 = 32
bitWidth NumberTypeF64 = 64

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
  | InsLocalGet Int
  | InsLocalSet Int
  | InsLocalTee Int
  | InsGlobalGet Int
  | InsGlobalSet Int
  | InsTableGet Int
  | InsTableSet Int
  | InsTableSize Int
  | InsTableGrow Int
  | InsTableFill Int
  | InsTableCopy Int Int
  | InsTableInit Int Int
  | InsElemDrop Int
  | InsTMemLoad NumberType MemArg (Maybe IntStoreSize) (Maybe IntSign)
  | InsTMemStore NumberType MemArg (Maybe IntStoreSize) (Maybe IntSign)
  | InsMemSize
  | InsMemGrow
  | InsMemFill
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
