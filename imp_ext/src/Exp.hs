module Exp (Exp(..), exp', get) where

import Common
import Test.QuickCheck

data Exp = EVar Variable
         | EInt Int
         | EAdd Exp Exp
         | EMul Exp Exp
         | ESub Exp Exp
         | EDiv Exp Exp
         | EMod Exp Exp
         | EPow Exp Exp
         | BExp Bool
         | BEq Exp Exp
         | BLt Exp Exp
         | BGt Exp Exp
         | BLte Exp Exp
         | BGte Exp Exp
         | BAnd Exp Exp
         | BOr Exp Exp
         | BNot Exp
    deriving Show

instance Arbitrary Exp where
  arbitrary = sized arb
    where
      arb 0 = EVar <$> arbitrary
      arb n = frequency [ (1, EVar <$> arbitrary)
                        , (1, EInt <$> arbitrary)
                        , (1, BExp <$> arbitrary)
                        , (1, BNot <$> arb (n - 1))
                        , (2, EAdd <$> arbHalf <*> arbHalf) 
                        , (2, EMul <$> arbHalf <*> arbHalf) 
                        , (2, ESub <$> arbHalf <*> arbHalf) 
                        , (2, EDiv <$> arbHalf <*> arbHalf) 
                        , (2, EMod <$> arbHalf <*> arbHalf) 
                        , (2, EPow <$> arbHalf <*> arbHalf) 
                        , (2, BEq <$> arbHalf <*> arbHalf) 
                        , (2, BLt <$> arbHalf <*> arbHalf) 
                        , (2, BGt <$> arbHalf <*> arbHalf) 
                        , (2, BLte <$> arbHalf <*> arbHalf) 
                        , (2, BGte <$> arbHalf <*> arbHalf) 
                        , (2, BAnd <$> arbHalf <*> arbHalf)
                        , (2, BOr <$> arbHalf <*> arbHalf) 
                        ]
        where arbHalf = arb (n `div` 2)

-- cauta Value unei variabile si intoarce MyState
-- intr-un tuplu
get :: MyState ->  Variable -> (Value, MyState)
get (List sigma) v = case lookup v sigma of
    Just val -> (val, List sigma)
    Nothing -> (VInt 0, List sigma)

getInt :: Value -> Int
getInt (VInt i) = i
getInt _ = 0 -- Default value on type error

getBool :: Value -> Bool
getBool (VBool b) = b
getBool _ = False

exp' :: MyState -> Exp -> (Value, MyState)
exp' sigma (EVar v) = get sigma v
exp' sigma (EInt i) = (VInt i, sigma)
exp' sigma (EAdd e1 e2) =
    let (v1, sigma1) = exp' sigma e1
        (v2, sigma2) = exp' sigma1 e2
    in (VInt (getInt v1 + getInt v2), sigma2)
exp' sigma (EMul e1 e2) =
    let (v1, sigma1) = exp' sigma e1
        (v2, sigma2) = exp' sigma1 e2
    in (VInt (getInt v1 * getInt v2), sigma2)
exp' sigma (EDiv e1 e2) =
    let (v1, sigma1) = exp' sigma e1
        (v2, sigma2) = exp' sigma1 e2
        d = getInt v2
    in if d == 0 then (VInt 0, sigma2) else (VInt (getInt v1 `div` d), sigma2)
exp' sigma (ESub e1 e2) =
    let (v1, sigma1) = exp' sigma e1
        (v2, sigma2) = exp' sigma1 e2
    in (VInt (getInt v1 - getInt v2), sigma2)
exp' sigma (EMod e1 e2) =
    let (v1, sigma1) = exp' sigma e1
        (v2, sigma2) = exp' sigma1 e2
        d = getInt v2
    in if d == 0 then (VInt 0, sigma2) else (VInt (getInt v1 `mod` d), sigma2)
exp' sigma (EPow e1 e2) =
    let (v1, sigma1) = exp' sigma e1
        (v2, sigma2) = exp' sigma1 e2
        val2 = getInt v2
    in (VInt (getInt v1 ^ (if val2 < 0 then 0 else val2)), sigma2)
exp' sigma (BEq e1 e2) =
    let (v1, sigma1) = exp' sigma e1
        (v2, sigma2) = exp' sigma1 e2
    in (VBool (getInt v1 == getInt v2), sigma2)
exp' sigma (BLt e1 e2) =
    let (v1, sigma1) = exp' sigma e1
        (v2, sigma2) = exp' sigma1 e2
    in (VBool (getInt v1 < getInt v2), sigma2)
exp' sigma (BGt e1 e2) =
    let (v1, sigma1) = exp' sigma e1
        (v2, sigma2) = exp' sigma1 e2
    in (VBool (getInt v1 > getInt v2), sigma2)
exp' sigma (BLte e1 e2) =
    let (v1, sigma1) = exp' sigma e1
        (v2, sigma2) = exp' sigma1 e2
    in (VBool (getInt v1 <= getInt v2), sigma2)
exp' sigma (BGte e1 e2) =
    let (v1, sigma1) = exp' sigma e1
        (v2, sigma2) = exp' sigma1 e2
    in (VBool (getInt v1 >= getInt v2), sigma2)
exp' sigma (BAnd b1 b2) =
    let (v1, sigma1) = exp' sigma b1
        (v2, sigma2) = exp' sigma1 b2
    in (VBool (getBool v1 && getBool v2), sigma2)
exp' sigma (BOr b1 b2) =
    let (v1, sigma1) = exp' sigma b1
        (v2, sigma2) = exp' sigma1 b2
    in (VBool (getBool v1 || getBool v2), sigma2)
exp' sigma (BNot b) =
    let (v1, sigma1) = exp' sigma b
    in (VBool (not (getBool v1)), sigma1)
exp' sigma (BExp b) = (VBool b, sigma)
