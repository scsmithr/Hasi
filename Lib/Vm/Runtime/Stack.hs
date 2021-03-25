module Lib.Vm.Runtime.Stack where

class Stack s where
  push :: s -> a -> s
  pop :: s -> Maybe a
