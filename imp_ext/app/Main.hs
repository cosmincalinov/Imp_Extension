-- TODO: rezolvat problema cu import-urile (DONE: deschis in locul prost)
-- TODO: de tradus tot in engleza (DONE)
-- TODO: impartirea in fisiere/module (DONE)
-- TODO: unit testing cu HUnit (DONE)
-- TODO: testare paralelism (KINDA DONE)
-- TODO: compilare si rulare cu ghc pt a specifica nr de procese 
-- TODO: poate adaugarea mai multor tipuri de date (char, string, float)
-- TODO: monada state pt env

module Main where

import Common
import Exp
import Stmt

factorialProg :: Stmt
factorialProg = 
  Block 
    [ Assign (Var "n") (EInt 5)
    , Assign (Var "res") (EInt 1)
    , While (BGt (EVar (Var "n")) (EInt 0))
        (Block
           [ Assign (Var "res") (EMul (EVar (Var "res")) (EVar (Var "n")))
           , Assign (Var "n") (ESub (EVar (Var "n")) (EInt 1))
           ]
        )
    ]

main :: IO ()
main = do
  let x = Var "x"
      y = Var "y"
      example = Seq
        (Assign x (EInt 0))
        (While (BLt (EVar x) (EInt 3)) (Block
          [ Assign y (EAdd (EVar x) (EInt 1))
          , Assign x (EAdd (EVar x) (EInt 1))
          ]))

      final = stmt (List []) example
      (val, _) = get final y

  putStrLn $ "Result of program (value of y): " ++ show val ++ "\n"

  -- factorial
  let finalFact = stmt (List []) factorialProg
      (res, _) = get finalFact (Var "res")

  putStrLn $ "Result of factorial is: " ++ show res ++ "\n"

