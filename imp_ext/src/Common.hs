module Common where

import Test.QuickCheck

newtype Variable = Var String deriving (Eq, Ord, Show)

data Value = VInt Int | VBool Bool deriving (Eq, Show)

newtype MyState = List [(Variable, Value)] deriving (Eq, Show)

instance Arbitrary Variable where
  arbitrary = do
    c <- elements ['a'..'z']
    return $ Var [c]

instance Arbitrary Value where
  arbitrary = do
    vInt <- arbitrary
    vBool <- arbitrary
    elements [VInt vInt, VBool vBool]

instance Arbitrary MyState where
  arbitrary = List <$> arbitrary
