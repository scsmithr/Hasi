module Lib.Structure where

import qualified Data.ByteString as B
import Data.Int
import Data.Word

data ScalarValue
  = ByteValue Word8
  | U32Value Word32
  | U64Value Word64
  | S32Value Int32
  | S64Value Int64
  | I32Value Word32 -- uninterpreted
  | I64Value Word64 -- uninterpreted
  deriving (Show, Eq)

newtype Name = Name B.ByteString deriving (Show, Eq)

data FloatValue
  = F32Value Double
  | F64Value Double
  deriving (Show, Eq)

data NumberType = I32 | I64 | F32 | F64 deriving (Show, Eq)

data RefType = FuncRef | ExternRef deriving (Show, Eq)

data ValueType = NumberType | RefType deriving (Show, Eq)

newtype ResultType = ResultType [ValueType] deriving (Show, Eq)

newtype FuncType = FuncType (ResultType, ResultType) deriving (Show, Eq)

returnArity :: FuncType -> Int
returnArity (FuncType (_, ResultType rets)) = length rets

funcArity :: FuncType -> Int
funcArity (FuncType (ResultType is, _)) = length is

data Limit
  = LowerBoundLimit Word32
  | BoundedLimit (Word32, Word32)
  deriving (Show, Eq)

newtype MemType = MemType Limit deriving (Show, Eq)

newtype TableType = TableType (Limit, RefType) deriving (Show, Eq)

data GlobalType
  = ConstGlobal ValueType
  | MutGlobal ValueType
  deriving (Show, Eq)

data ExtType
  = ExtFunc FuncType
  | ExtTable TableType
  | ExtMem MemType
  | ExtGlobal GlobalType
  deriving (Show, Eq)

data Instr = I32Add | I32Sub deriving (Show, Eq)

type Expr = [Instr]

type Idx = Word32

data Func = Func {fType :: Idx, fLocals :: [ValueType], fBody :: Expr} deriving (Show, Eq)

newtype Table = Table {tType :: TableType} deriving (Show, Eq)

newtype Mem = Mem {mType :: MemType} deriving (Show, Eq)

data Global = Global {gType :: GlobalType, gInit :: Expr} deriving (Show, Eq)

data ElemMode
  = ElemModePassive
  | ElemModeActive {elemModeTable :: Idx, elemModeOffset :: Expr}
  | Declarative
  deriving (Show, Eq)

data Elem = Elem {eType :: RefType, eInit :: [Expr], eMode :: ElemMode} deriving (Show, Eq)

data DataMode = Passive | Active {dataModeMemory :: Idx, dataModeOffset :: Expr} deriving (Show, Eq)

data Data = Data {dataInit :: B.ByteString, dataMode :: DataMode} deriving (Show, Eq)

data ImportDesc
  = ImportFunc Idx
  | ImportTable TableType
  | ImportMem MemType
  | ImportGlobal GlobalType
  deriving (Show, Eq)

data Import = Import {imModule :: Name, imDesc :: ImportDesc} deriving (Show, Eq)

data ExportDesc
  = ExportFunc Idx
  | ExportTable Idx
  | ExportMem Idx
  | ExportGlobal Idx
  deriving (Show, Eq)

data Export = Export {exName :: Name, exDesc :: ExportDesc} deriving (Show, Eq)

data Module = Module
  { mTypes :: [FuncType],
    mFuncs :: [Func],
    mTables :: [Table],
    mMems :: [Mem],
    mGlobals :: [Global],
    mElems :: [Elem],
    mDatas :: [Data],
    mStartFunc :: Maybe Idx,
    mImports :: [Import],
    mExports :: [Export]
  }
  deriving (Show, Eq)
