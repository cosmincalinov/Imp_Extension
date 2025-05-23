-- TODO: rezolvat problema cu import-urile (DONE: deschis in locul prost)
-- TODO: de tradus tot in engleza (DONE)
-- TODO: impartirea in fisiere/module (DONE)
-- TODO: unit testing cu HUnit
-- TODO: testare paralelism
-- TODO: compilare si rulare cu ghc pt a specifica nr de procese
-- TODO: poate adaugarea mai multor tipuri de date (char, string, float)
-- TODO: monada state pt env

module Main where

-- import Test.HUnit
-- import Exp (Exp(..), get)
-- import Stmt (stmt)

-- -- Example test
-- test1 :: Test
-- test1 = TestCase $
--   let x = Var "x"
--       stmt1 = Assign x (EInt 1)
--       result = stmt (List []) stmt1
--       (val, _) = get result x
--   in assertEqual "Assign x = 1" (VInt 1) val

-- tests :: Test
-- tests = TestList [TestLabel "Test 1" test1]

main :: IO ()
main = putStrLn "Testing..."

