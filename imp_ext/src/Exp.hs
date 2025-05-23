module Exp (Exp(..), exp', get) where

import Common

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

-- cauta Value unei variabile si intoarce MyState
-- intr-un tuplu
get :: MyState ->  Variable -> (Value, MyState)
get (List sigma) v = case lookup v sigma of
    Just val -> (val, List sigma)
    Nothing -> (VInt 0, List sigma)

exp' :: MyState -> Exp -> (Value, MyState)
exp' sigma (EVar v) = get sigma v
exp' sigma (EInt i) = (VInt i, sigma)
exp' sigma (EAdd e1 e2) =
    let (VInt v1, sigma1) = exp' sigma e1
        (VInt v2, sigma2) = exp' sigma1 e2
    in (VInt (v1 + v2), sigma2)
exp' sigma (EMul e1 e2) =
    let (VInt v1, sigma1) = exp' sigma e1
        (VInt v2, sigma2) = exp' sigma1 e2
    in (VInt (v1 * v2), sigma2)
exp' sigma (EDiv e1 e2) =
    let (VInt v1, sigma1) = exp' sigma e1
        (VInt v2, sigma2) = exp' sigma1 e2
    in (VInt (v1 `div` v2), sigma2)
exp' sigma (ESub e1 e2) =
    let (VInt v1, sigma1) = exp' sigma e1
        (VInt v2, sigma2) = exp' sigma1 e2
    in (VInt (v1 - v2), sigma2)
exp' sigma (EMod e1 e2) =
    let (VInt v1, sigma1) = exp' sigma e1
        (VInt v2, sigma2) = exp' sigma1 e2
    in (VInt (v1 `mod` v2), sigma2)
exp' sigma (EPow e1 e2) =
    let (VInt v1, sigma1) = exp' sigma e1
        (VInt v2, sigma2) = exp' sigma1 e2
    in (VInt (v1 ^ v2), sigma2)
exp' sigma (BEq e1 e2) =
    let (VInt v1, sigma1) = exp' sigma e1
        (VInt v2, sigma2) = exp' sigma1 e2
    in (VBool (v1 == v2), sigma2)
exp' sigma (BLt e1 e2) =
    let (VInt v1, sigma1) = exp' sigma e1
        (VInt v2, sigma2) = exp' sigma1 e2
    in (VBool (v1 < v2), sigma2)
exp' sigma (BGt e1 e2) =
    let (VInt v1, sigma1) = exp' sigma e1
        (VInt v2, sigma2) = exp' sigma1 e2
    in (VBool (v1 > v2), sigma2)
exp' sigma (BLte e1 e2) =
    let (VInt v1, sigma1) = exp' sigma e1
        (VInt v2, sigma2) = exp' sigma1 e2
    in (VBool (v1 <= v2), sigma2)
exp' sigma (BGte e1 e2) =
    let (VInt v1, sigma1) = exp' sigma e1
        (VInt v2, sigma2) = exp' sigma1 e2
    in (VBool (v1 >= v2), sigma2)
exp' sigma (BAnd b1 b2) =
    let (VBool v1, sigma1) = exp' sigma b1
        (VBool v2, sigma2) = exp' sigma1 b2
    in (VBool (v1 && v2), sigma2)
exp' sigma (BOr b1 b2) =
    let (VBool v1, sigma1) = exp' sigma b1
        (VBool v2, sigma2) = exp' sigma1 b2
    in (VBool (v1 || v2), sigma2)
exp' sigma (BNot b) =
    let (VBool v1, sigma1) = exp' sigma b
    in (VBool (not v1), sigma1)
exp' sigma (BExp b) = (VBool b, sigma)
exp' _ _ = error "Nevalid" -- dar de acesta este nevoie ?
