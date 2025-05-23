module Common where

newtype Variable = Var String deriving (Eq, Ord, Show)

data Value = VInt Int | VBool Bool deriving (Eq, Show)

newtype MyState = List [(Variable, Value)]
