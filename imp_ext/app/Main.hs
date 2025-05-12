module Main where
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import qualified Data.Map as M
import GHC.IO (unsafePerformIO)
import Control.Monad (forM_)
-- import Test.HUnit


newtype Variabila = Var String deriving(Eq, Ord, Show)

data Valoare = VInt Int 
             | VBool Bool
             deriving(Show)

newtype Stare = List [(Variabila, Valoare)]

newtype MyType = MList (Variabila, Valoare)

data Exp = EVar Variabila
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

-- cauta valoarea unei variabile si intoarce starea
-- intr-un tuplu
get :: Stare ->  Variabila -> (Valoare, Stare)
get (List sigma) v = case lookup v sigma of
    Just val -> (val, List sigma)
    Nothing -> (VInt 0, List sigma)

exp' :: Stare -> Exp -> (Valoare, Stare)
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

data Stmt = Assign Variabila Exp -- operatorul = din alte limbaje
          | Seq Stmt Stmt -- pt a inlantui stmt-uri
          | If Exp Stmt Stmt -- 2 Stmt si pentru else
          | While Exp Stmt
          | Skip
          | Block [Stmt]
          | Par [Stmt]

-- schimba starea, adaugand sau inlocuind variabile
set :: Stare -> Variabila -> Valoare -> Stare
set (List sigma) setVar setVal =
    case sigma of
        [] ->  List [(setVar, setVal)] 
        (var, val) : other -> 
            if var == setVar
            then List ((setVar, setVal) : other)
            else case set (List other) setVar setVal of
                List updated -> List ((var,val) : updated)

stmt :: Stare -> Stmt -> Stare
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
        _ -> sigma -- TODO: eroare aici in loc de stare
stmt sigma (While expr stmt1) =
    case exp' sigma expr of
        (VBool True, _) -> let sigma2 = stmt sigma stmt1
            in stmt sigma2 (While expr stmt1)
        (VBool False, _) -> sigma 
stmt sigma Skip = sigma
stmt sigma (Block []) = sigma
stmt sigma (Block (stmt1 : rest)) =
    let sigma2 = stmt sigma stmt1
    in stmt sigma2 (Block rest)
stmt sigma (Par stmts) = unsafePerformIO (runParallel sigma stmts)

-- poate inspirat din Orc
-- Par Stmt1 Stmt2
-- Par [Stmt], trb facut join
-- deschid un forkIO(), vad cum recuperez rezultatul
merge :: Stare -> Stare -> Stare
merge (List sigma1) (List sigma2) =
    let m1 = M.fromList sigma1
        m2 = M.fromList sigma2
        merged = M.union m2 m1 
    in List (M.toList merged)

runParallel :: Stare -> [Stmt] -> IO Stare
runParallel sigma stmts = do
    shared <- newMVar sigma
    doneVars <- mapM (const newEmptyMVar) stmts
    forM_ (zip stmts doneVars) $ \(stmti, done) -> forkIO $ do
        let partial = stmt sigma stmti
        modifyMVar_ shared $ \global -> return (merge global partial)
        putMVar done ()
    mapM_ takeMVar doneVars
    readMVar shared

main :: IO ()
main = do
    -- Test 1 
    -- let x = Var "x"
    --     stmt1 = Assign x (EInt 1)
    --     stmt2 = Assign x (EInt 2)
    --     program = Seq stmt1 stmt2
    --     stare = List []
    --     finalState = stmt stare program
    --     (val, _) = get finalState x
    -- print val

    -- Test 2
    let x = Var "x"
        y = Var "y"
        stmt1 = Assign x (EInt 0)
        stmt2 = While (BLt (EVar x) (EInt 3)) (Block [
            Assign y (EAdd (EVar x) (EInt 1)),
            Assign x (EAdd (EVar x) (EInt 1)) ])
        program = Seq stmt1 stmt2
        stare = List []
        finalState = stmt stare program
        (val, _) = get finalState y
    print val

    -- Test 3
    -- let x = Var "x"
    --     y = Var "y"
    --     p = Par [Assign x (EInt 1), Assign y (EInt 2)]
    --     s = stmt (List []) p
    --     (vx, _) = get s x
    --     (vy, _) = get s y
    -- print vx
    -- print vy

