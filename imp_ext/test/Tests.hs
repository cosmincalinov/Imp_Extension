module Tests where

import Test.HUnit
import Exp
import Stmt
import Common

stmt1 :: Stmt
stmt1 = Assign (Var "x") (EInt 1)

stmt2 :: Stmt
stmt2 = Assign (Var "x") (EInt 2)

program1 :: Stmt
program1 = Seq stmt1 stmt2

expected1 :: Value
expected1 = VInt 2

actual1 :: Value
actual1 = fst (get (stmt (List []) program1) (Var "x"))

stmt3 :: Stmt
stmt3 = Assign (Var "x") (EInt 0)

stmt4 :: Stmt
stmt4 = While (BLt (EVar (Var "x")) (EInt 3)) (Block
  [ Assign (Var "y") (EAdd (EVar (Var "x")) (EInt 1))
  , Assign (Var "x") (EAdd (EVar (Var "x")) (EInt 1))
  ])

program2 :: Stmt
program2 = Seq stmt3 stmt4

expected2 :: Value
expected2 = VInt 3

actual2 :: Value
actual2 = fst (get (stmt (List []) program2) (Var "y"))

stmt5 :: Stmt
stmt5 = Par [Assign (Var "x") (EInt 1), Assign (Var "y") (EInt 2)]

expected3x :: Value
expected3x = VInt 1
expected3y :: Value
expected3y = VInt 2

finalState3 = stmt (List []) stmt5
actual3x :: Value
actual3x = fst (get finalState3 (Var "x"))
actual3y :: Value
actual3y = fst (get finalState3 (Var "y"))

tests :: Test
tests = TestList
  [ "Seq Assignment" ~: actual1 ~?= expected1
  , "While Loop"     ~: actual2 ~?= expected2
  , "Parallel x"     ~: actual3x ~?= expected3x
  , "Parallel y"     ~: actual3y ~?= expected3y
  ]

main :: IO ()
main = runTestTT tests >>= print
