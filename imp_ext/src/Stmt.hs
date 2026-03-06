module Stmt where

import Exp
import Common
import Control.Parallel.Strategies
import Test.QuickCheck

data Stmt = Assign Variable Exp -- operatorul = din alte limbaje
          | Seq Stmt Stmt -- pt a inlantui stmt-uri
          | If Exp Stmt Stmt -- 2 Stmt si pentru else
          | While Exp Stmt
          | Skip
          | Block [Stmt]
          | Par [Stmt]
    deriving Show

instance Arbitrary Stmt where
  arbitrary = sized arb
    where
      arb 0 = oneof [return Skip, Assign <$> arbitrary <*> arbitrary]
      arb n = frequency [ (1, return Skip)
                        , (1, Assign <$> arbitrary <*> arbitrary)
                        , (2, Seq <$> arbHalf <*> arbHalf)
                        , (2, If <$> arbitrary <*> arbHalf <*> arbHalf)
                        -- , (1, While <$> arbitrary <*> arbHalf)
                        , (2, Block <$> resize (n `div` 2) arbitrary)
                        , (2, Par <$> resize (n `div` 2) arbitrary)
                        ]
        where arbHalf = arb (n `div` 2)

-- schimba MyState, adaugand sau inlocuind variabile
set :: MyState -> Variable -> Value -> MyState
set (List sigma) setVar setVal =
    case sigma of
        [] ->  List [(setVar, setVal)]
        (var, val) : other ->
            if var == setVar
            then List ((setVar, setVal) : other)
            else case set (List other) setVar setVal of
                List updated -> List ((var,val) : updated)

stmt :: MyState -> Stmt -> MyState
stmt sigma (Seq s1 s2) =
    let sigma' = stmt sigma s1
    in stmt sigma' s2
stmt sigma (Assign var expr) =
    let (val, sigma') = exp' sigma expr
    in set sigma' var val
stmt sigma (If expr stmt1 stmt2) =
    case exp' sigma expr of
        (VBool True, _) -> stmt sigma stmt1
        (VBool False, _) -> stmt sigma stmt2
        _ -> sigma -- daca conditia nu e Bool, o ignoram (ca False)
stmt sigma (While expr stmt1) =
    case exp' sigma expr of
        (VBool True, _) -> let sigma2 = stmt sigma stmt1
            in stmt sigma2 (While expr stmt1)
        (VBool False, _) -> sigma
        _ -> sigma -- opreste bucla daca conditia nu e Bool
stmt sigma Skip = sigma
stmt sigma (Block []) = sigma
stmt sigma (Block (stmt1 : rest)) =
    let sigma2 = stmt sigma stmt1
    in stmt sigma2 (Block rest)
stmt sigma (Par stmts) =
    let partials = withStrategy (parList rpar $!) (map (stmt sigma) stmts)
    -- TODO: verifica daca modificarile sunt consistente sau evaluare de expresii in paralel
    in foldl myMerge (List []) partials -- facem merge la fiecare MyState

removeDups :: [(Variable, Value)] -> [(Variable, Value)]
removeDups = go []
  where
    go seen [] = seen -- am vazut tot -> nu mai exista duplicate in lista, 
    -- intoarcem list
    go seen ((k,v):rest) -- destructuram primul tuplu din lista actuala
      | k `elem` map fst seen = go seen rest -- daca se gaseste il
      -- ignoram / eliminam
      | otherwise             = go (seen ++ [(k,v)]) rest -- daca nu
      -- il concatenam cu seen

myMerge :: MyState -> MyState -> MyState
myMerge (List s1) (List s2) =
  List (removeDups (s2 ++ s1))
-- pentru a face merge va trebui sa eliminam duplicatele
-- in urma concatenarii pentru a nu aparea confilcte
-- ex.: definirea unei variabile cu acelasi nume in
-- 2 stmt separate

-- s2, inainte lui s1, deoarece schimbarile finale
-- ale starii au o prioritate mai mare