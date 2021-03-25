module Lib.Vm.Structure.Types where

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

newtype Name = Name B.ByteString

data FloatValue
  = F32Value Double
  | F64Value Double

data NumberType = I32 | I64 | F32 | F64

data RefType = FuncRef | ExternRef

data ValueType = NumberType | RefType

newtype ResultType = ResultType [ValueType]

newtype FuncType = FuncType (ResultType, ResultType)

data Limit
  = LowerBoundLimit Word32
  | BoundedLimit (Word32, Word32)

newtype MemType = MemType Limit

newtype TableType = TableType (Limit, RefType)

data GlobalType
  = ConstGlobal ValueType
  | MutGlobal ValueType

data ExtType
  = ExtFunc FuncType
  | ExtTable TableType
  | ExtMem MemType
  | ExtGlobal GlobalType

data Instr = I32Add | I32Sub

type Expr = [Instr]

type Idx = Word32

data Func = Func {fType :: Idx, fLocals :: [ValueType], fBody :: Expr}

newtype Table = Table {tType :: TableType}

newtype Mem = Mem {mType :: MemType}

data Global = Global {gType :: GlobalType, gInit :: Expr}

data ElemMode
  = ElemModePassive
  | ElemModeActive {elemModeTable :: Idx, elemModeOffset :: Expr}
  | Declarative

data Elem = Elem {eType :: RefType, eInit :: [Expr], eMode :: ElemMode}

data DataMode = Passive | Active {dataModeMemory :: Idx, dataModeOffset :: Expr}

data Data = Data {dataInit :: B.ByteString, dataMode :: DataMode}

data ImportDesc
  = ImportFunc Idx
  | ImportTable TableType
  | ImportMem MemType
  | ImportGlobal GlobalType

data Import = Import {imModule :: Name, imDesc :: ImportDesc}

data ExportDesc
  = ExportFunc Idx
  | ExportTable Idx
  | ExportMem Idx
  | ExportGlobal Idx

data Export = Export {exName :: Name, exDesc :: ExportDesc}

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
