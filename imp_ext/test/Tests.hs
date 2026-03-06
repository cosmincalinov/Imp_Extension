module Main where

import Test.HUnit
import Exp
import Stmt
import Common
import Control.Monad (void)
import Test.QuickCheck

-- Test 1: Seq Assignment
stmt1 = Assign (Var "x") (EInt 1)
stmt2 = Assign (Var "x") (EInt 2)
program1 = Seq stmt1 stmt2
expected1 = VInt 2
actual1 = fst (get (stmt (List []) program1) (Var "x"))

-- Test 2: While Loop
stmt3 = Assign (Var "x") (EInt 0)
stmt4 = While (BLt (EVar (Var "x")) (EInt 3)) (Block
  [ Assign (Var "y") (EAdd (EVar (Var "x")) (EInt 1))
  , Assign (Var "x") (EAdd (EVar (Var "x")) (EInt 1))
  ])
program2 = Seq stmt3 stmt4
expected2 = VInt 3
actual2 = fst (get (stmt (List []) program2) (Var "y"))

-- Test 3: Parallel
stmt5 = Par [Assign (Var "x") (EInt 1), Assign (Var "y") (EInt 2)]
finalState3 = stmt (List []) stmt5
expected3x = VInt 1
expected3y = VInt 2
actual3x = fst (get finalState3 (Var "x"))
actual3y = fst (get finalState3 (Var "y"))

tests :: Test
tests = TestList
  [ "Seq Assignment" ~: actual1 ~?= expected1
  , "While Loop"     ~: actual2 ~?= expected2
  , "Parallel x"     ~: actual3x ~?= expected3x
  , "Parallel y"     ~: actual3y ~?= expected3y
  ]

-- properties
prop_skip_doesnt_modify_state :: MyState -> Bool
prop_skip_doesnt_modify_state sigma =
  let sigma' = stmt sigma Skip 
  in sigma == sigma'
   
prop_exec_secv_is_comp :: MyState -> Stmt -> Stmt -> Bool
prop_exec_secv_is_comp sigma stmt1 stmt2 = 
  stmt sigma (Seq stmt1 stmt2) == stmt (stmt sigma stmt1) stmt2

prop_assoc_seq_comp :: MyState -> Stmt -> Stmt -> Stmt -> Bool
prop_assoc_seq_comp sigma stmt1 stmt2 stmt3 =
  stmt sigma (Seq (Seq stmt1 stmt2) stmt3) == stmt sigma (Seq stmt1 (Seq stmt2 stmt3))

prop_assign_check :: MyState -> Variable -> Int -> Bool
prop_assign_check sigma x i = 
    let expr = EInt i
        stmt_assign = Assign x expr
        final_state = stmt sigma stmt_assign
        (val, _) = get final_state x
    in val == VInt i

prop_determinism :: MyState -> Stmt -> Bool
prop_determinism sigma st =
    let run1 = stmt sigma st
        run2 = stmt sigma st
    in run1 == run2

main :: IO ()
main = do
  void (runTestTT tests)
  quickCheck prop_skip_doesnt_modify_state
  quickCheck prop_exec_secv_is_comp
  quickCheck prop_assoc_seq_comp
  quickCheck prop_assign_check
  quickCheck prop_determinism
