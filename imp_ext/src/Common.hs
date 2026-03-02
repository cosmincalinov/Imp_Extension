module Common where

import Test.QuickCheck

newtype Variable = Var String deriving (Eq, Ord, Show)

data Value = VInt Int | VBool Bool deriving (Eq, Show)

newtype MyState = List [(Variable, Value)]

instance Arbitrary Variable where
  arbitrary = Var <$> arbitrary

instance Arbitrary Value where
  arbitrary = do
    vInt <- arbitrary
    vBool <- arbitrary
    elements [VInt vInt, VBool vBool]