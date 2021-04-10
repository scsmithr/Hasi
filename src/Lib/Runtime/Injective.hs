{-# LANGUAGE MultiParamTypeClasses #-}

module Lib.Runtime.Injective where

import Data.Int
import Data.Word
import qualified Lib.Runtime.Structure as RS

-- | Describes a type that may be converted from a to b.
class Injective a b where
  to :: a -> b

instance Injective Word32 RS.NumberValue where to = RS.IntValue . RS.U32

instance Injective Word64 RS.NumberValue where to = RS.IntValue . RS.U64

instance Injective Int32 RS.NumberValue where to = RS.IntValue . RS.I32

instance Injective Int64 RS.NumberValue where to = RS.IntValue . RS.I64

instance Injective Float RS.NumberValue where to = RS.FloatValue . RS.F32

instance Injective Double RS.NumberValue where to = RS.FloatValue . RS.F64

instance Injective RS.NumberValue RS.StackEntry where to = RS.StackValue . RS.Number

instance Injective Word32 RS.StackEntry where to = to . (to :: Word32 -> RS.NumberValue)

instance Injective Word64 RS.StackEntry where to = to . (to :: Word64 -> RS.NumberValue)

instance Injective Int32 RS.StackEntry where to = to . (to :: Int32 -> RS.NumberValue)

instance Injective Int64 RS.StackEntry where to = to . (to :: Int64 -> RS.NumberValue)

instance Injective Float RS.StackEntry where to = to . (to :: Float -> RS.NumberValue)

instance Injective Double RS.StackEntry where to = to . (to :: Double -> RS.NumberValue)
