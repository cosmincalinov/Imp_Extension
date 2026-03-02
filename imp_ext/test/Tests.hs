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

main :: IO ()
main = do
  void (runTestTT tests)
  quickCheck prop_skip_doesnt_modify_state
  quickCheck prop_exec_secv_is_comp
