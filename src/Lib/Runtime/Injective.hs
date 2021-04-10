{-# LANGUAGE MultiParamTypeClasses #-}

module Lib.Runtime.Injective where

import Data.Int
import Data.Word
import qualified Lib.Runtime.Structure as RS

-- | Describes a type that can be converted from a to b.
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

-- | Describes a type may be converted from type a to b.
class MaybeInjective a b where
  toMaybe :: a -> Maybe b

instance MaybeInjective RS.NumberValue Word32 where
  toMaybe (RS.IntValue (RS.U32 v)) = Just v
  toMaybe _ = Nothing

instance MaybeInjective RS.NumberValue Word64 where
  toMaybe (RS.IntValue (RS.U64 v)) = Just v
  toMaybe _ = Nothing

instance MaybeInjective RS.NumberValue Int32 where
  toMaybe (RS.IntValue (RS.I32 v)) = Just v
  toMaybe _ = Nothing

instance MaybeInjective RS.NumberValue Int64 where
  toMaybe (RS.IntValue (RS.I64 v)) = Just v
  toMaybe _ = Nothing

instance MaybeInjective RS.NumberValue Float where
  toMaybe (RS.FloatValue (RS.F32 v)) = Just v
  toMaybe _ = Nothing

instance MaybeInjective RS.NumberValue Double where
  toMaybe (RS.FloatValue (RS.F64 v)) = Just v
  toMaybe _ = Nothing
