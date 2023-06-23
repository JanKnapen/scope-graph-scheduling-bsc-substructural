module Main where

import Test.HUnit

import Syntax
import qualified AlgJ as J
import qualified System.Exit as Exit
import Free.Logic.Equals
import Data.List (isPrefixOf)

runTCSuccessTest :: MLy -> Ty -> IO ()
runTCSuccessTest e t = case J.runTC e of
  Right (_, scheme) -> case scheme of
    J.Scheme _ t' ->
      if t == t'
        then return ()
        else assertFailure $ "Expected type " ++ show t ++ ", but got " ++ show t'
  Left err -> assertFailure $ "Type error: " ++ err

runTCFailureTest :: MLy -> String -> String -> IO ()
runTCFailureTest e failureMessage expectedErrorMessage = case J.runTC e of
  Right _ -> assertFailure failureMessage
  Left errorMessage
    | errorMessage == expectedErrorMessage -> return ()
    | otherwise -> assertFailure $ "Actual error message: \"" ++ errorMessage ++ "\" does not match with expected error message: \"" ++ expectedErrorMessage ++ "\""

runTCFailureUnificationTest :: MLy -> IO ()
runTCFailureUnificationTest e = case J.runTC e of
  Right _ -> assertFailure "Expected unification error"
  Left errorMessage
    | "Unification error" `isPrefixOf` errorMessage -> return ()
    | otherwise -> assertFailure "Expected unification error"

-- Non-substructural typing rules
-- Base
testBase1 = runTCSuccessTest (Num 0) numT
testBase2 = runTCFailureTest (Ident "x") "Expected no matching declarations found" "Variable x not found."
testBase3 = runTCFailureTest (Plus (Ident "x") (Ident "x")) "Expected no matching declarations found" "Variable x not found."
testBase4 = runTCSuccessTest (Plus (Num 0) (Num 1)) numT

-- Abs
testAbs1 = runTCSuccessTest (Abs "x" numT (Num 0)) (funT numT numT)
testAbs2 = runTCSuccessTest (Abs "x" numT (Ident "x")) (funT numT numT)
testAbs3 = runTCFailureTest (Abs "x" numT (Ident "y")) "Expected no matching declarations found" "Variable y not found."
testAbs4 = runTCSuccessTest (Abs "x" numT (Plus (Ident "x") (Ident "x"))) (funT numT numT)
testAbs5 = runTCFailureTest (Abs "x" numT (Plus (Ident "y") (Ident "y"))) "Expected no matching declarations found" "Variable y not found."
testAbs6 = runTCFailureTest (Abs "x" numT (Plus (Ident "x") (Ident "y"))) "Expected no matching declarations found" "Variable y not found."

testAbs7 = runTCSuccessTest (Abs "x" (funT numT numT) (Ident "x")) (funT (funT numT numT) (funT numT numT))
testAbs8 = runTCSuccessTest (Abs "x" (funT numT numT) (App (Ident "x") (Num 0))) (funT (funT numT numT) numT)
testAbs9 = runTCFailureUnificationTest (Abs "x" (funT numT numT) (Let "x" numT (Num 0) (App (Ident "x") (Ident "x"))))
testAbs10 = runTCSuccessTest (Abs "x" (funT numT numT) (Let "y" numT (Num 0) (App (Ident "x") (Ident "y")))) (funT (funT numT numT) numT)

-- App
testApp1 = runTCSuccessTest (App (Abs "x" numT (Num 0)) (Num 1)) numT
testApp2 = runTCSuccessTest (App (Abs "x" numT (Ident "x")) (Num 1)) numT
testApp3 = runTCFailureTest (App (Abs "x" numT (Ident "y")) (Num 1)) "Expected no matching declarations found" "Variable y not found."
testApp4 = runTCSuccessTest (App (Abs "x" numT (Plus (Ident "x") (Ident "x"))) (Num 1)) numT
testApp5 = runTCFailureTest (App (Abs "x" numT (Plus (Ident "y") (Ident "y"))) (Num 1)) "Expected no matching declarations found" "Variable y not found."
testApp6 = runTCFailureTest (App (Abs "x" numT (Plus (Ident "x") (Ident "y"))) (Num 1)) "Expected no matching declarations found" "Variable y not found."

testApp7 = runTCSuccessTest (App (App (Abs "x" (funT numT numT) (Ident "x")) (Abs "x" numT (Ident "x"))) (Num 0)) numT
testApp8 = runTCSuccessTest (App (Abs "x" (funT numT numT) (App (Ident "x") (Num 0))) (Abs "x" numT (Ident "x"))) numT
testApp9 = runTCFailureUnificationTest (App (Abs "x" (funT numT numT) (Let "x" numT (Num 0) (App (Ident "x") (Ident "x")))) (Abs "x" numT (Ident "x")))
testApp10 = runTCSuccessTest (App (Abs "x" (funT numT numT) (Let "y" numT (Num 0) (App (Ident "x") (Ident "y")))) (Abs "x" numT (Ident "x"))) numT

-- Let
testLet1 = runTCSuccessTest (Let "x" numT (Num 0) (Let "x" numT (Ident "x") (Ident "x"))) numT
testLet2 = runTCSuccessTest (Let "x" numT (Num 0) (Let "x" numT (Num 1) (Ident "x"))) numT
testLet3 = runTCSuccessTest (Let "x" numT (Num 0) (Let "x" numT (Num 1) (Plus (Ident "x") (Ident "x")))) numT
testLet4 = runTCSuccessTest (Let "x" numT (Num 0) (Let "x" numT (Ident "x") (Plus (Ident "x") (Ident "x")))) numT
testLet5 = runTCSuccessTest (Let "x" numT (Num 0) (Let "x" numT (Ident "x") (Num 1))) numT

testLet6 = runTCSuccessTest (Let "x" (funT numT numT) (Abs "x" numT (Ident "x")) (Let "x" numT (App (Ident "x") (Num 0)) (Ident "x"))) numT
testLet7 = runTCFailureTest (Let "x" (funT numT numT) (Abs "x" numT (Ident "x")) (Let "y" numT (App (Ident "x") (Ident "y")) (Ident "x"))) "Expected no matching declarations found" "Variable y not found."
testLet8 = runTCSuccessTest (Let "x" (funT numT numT) (Abs "x" numT (Ident "x")) (Let "y" numT (App (Ident "x") (Num 0)) (Ident "y"))) numT

-- Complex
testComplex1 = runTCSuccessTest (Let "x" numT (Num 0) (Let "f" (funT numT numT) (Abs "x" numT (Plus (Ident "x") (Ident "x"))) (App (Ident "f") (Num 1)))) numT
testComplex2 = runTCSuccessTest (Let "f" (funT numT numT) (Abs "x" numT (Let "x" numT (Num 0) (Plus (Ident "x") (Num 1)))) (App (Ident "f") (Num 2))) numT

-- Errors
testError1 = runTCFailureUnificationTest (Plus (Num 0) (Abs "x" numT (Ident "x")))
testError2 = runTCFailureUnificationTest (Plus (Abs "x" numT (Ident "x")) (Num 0))
testError3 = runTCFailureUnificationTest (Plus (Abs "x" numT (Ident "x")) (Abs "x" numT (Ident "x")))

testError4 = runTCFailureUnificationTest (App (Abs "x" numT (Ident "x")) (Abs "x" numT (Ident "x")))
testError5 = runTCFailureUnificationTest (App (Num 0) (Num 1))

testError6 = runTCFailureUnificationTest (Let "x" numT (Abs "x" numT (Ident "x")) (Ident "x"))
testError7 = runTCFailureUnificationTest (Let "x" (funT numT numT) (Num 0) (App (Ident "x") (Num 1)))

tests :: Test
tests = TestList
    [ "testBase1" ~: testBase1
    , "testBase2" ~: testBase2 
    , "testBase3" ~: testBase3 
    , "testBase4" ~: testBase4 
    , "testAbs1" ~: testAbs1 
    , "testAbs2" ~: testAbs2 
    , "testAbs3" ~: testAbs3 
    , "testAbs4" ~: testAbs4 
    , "testAbs5" ~: testAbs5 
    , "testAbs6" ~: testAbs6 
    , "testAbs7" ~: testAbs7 
    , "testAbs8" ~: testAbs8 
    , "testAbs9" ~: testAbs9 
    , "testAbs10" ~: testAbs10 
    , "testApp1" ~: testApp1 
    , "testApp2" ~: testApp2 
    , "testApp3" ~: testApp3 
    , "testApp4" ~: testApp4 
    , "testApp5" ~: testApp5 
    , "testApp6" ~: testApp6 
    , "testApp7" ~: testApp7 
    , "testApp8" ~: testApp8 
    , "testApp9" ~: testApp9 
    , "testApp10" ~: testApp10 
    , "testLet1" ~: testLet1 
    , "testLet2" ~: testLet2 
    , "testLet3" ~: testLet3 
    , "testLet4" ~: testLet4 
    , "testLet5" ~: testLet5 
    , "testLet6" ~: testLet6 
    , "testLet7" ~: testLet7 
    , "testLet8" ~: testLet8 
    , "testComplex1" ~: testComplex1 
    , "testComplex2" ~: testComplex2 
    , "testError1" ~: testError1 
    , "testError2" ~: testError2 
    , "testError3" ~: testError3 
    , "testError4" ~: testError4 
    , "testError5" ~: testError5 
    , "testError6" ~: testError6 
    , "testError7" ~: testError7 
    ]

-- Linear typing rules
-- Abs
testLinearAbs1 = runTCFailureTest (Abs "x" (linearT numT) (Num 0)) "Expected substructural typing error" "Lambda linear variable x not used."
testLinearAbs2 = runTCSuccessTest (Abs "x" (linearT numT) (Ident "x")) (funT numT numT)
testLinearAbs3 = runTCFailureTest (Abs "x" (linearT numT) (Ident "y")) "Expected no matching declarations found" "Variable y not found."
testLinearAbs4 = runTCFailureTest (Abs "x" (linearT numT) (Plus (Ident "x") (Ident "x"))) "Expected substructural typing error" "Variable x not found."
testLinearAbs5 = runTCFailureTest (Abs "x" (linearT numT) (Plus (Ident "y") (Ident "y"))) "Expected no matching declarations found" "Variable y not found."
testLinearAbs6 = runTCFailureTest (Abs "x" (linearT numT) (Plus (Ident "x") (Ident "y"))) "Expected no matching declarations found" "Variable y not found."

testLinearAbs7 = runTCSuccessTest (Abs "x" (funT (linearT numT) numT) (Ident "x")) (funT (funT (linearT numT) numT) (funT (linearT numT) numT))
testLinearAbs8 = runTCSuccessTest (Abs "x" (funT (linearT numT) numT) (App (Ident "x") (Num 0))) (funT (funT (linearT numT) numT) numT)
testLinearAbs9 = runTCFailureTest (Abs "x" (funT (linearT numT) numT) (Let "x" (linearT numT) (Num 0) (App (Ident "x") (Ident "x")))) "Expected substructural typing error" "Variable x not found."
testLinearAbs10 = runTCSuccessTest (Abs "x" (funT (linearT numT) numT) (Let "y" (linearT numT) (Num 0) (App (Ident "x") (Ident "y")))) (funT (funT (linearT numT) numT) numT)

-- App
testLinearApp1 = runTCFailureTest (App (Abs "x" (linearT numT) (Num 0)) (Num 1)) "Expected substructural typing error" "Lambda linear variable x not used."
testLinearApp2 = runTCSuccessTest (App (Abs "x" (linearT numT) (Ident "x")) (Num 1)) numT
testLinearApp3 = runTCFailureTest (App (Abs "x" (linearT numT) (Ident "y")) (Num 1)) "Expected no matching declarations found" "Variable y not found."
testLinearApp4 = runTCFailureTest (App (Abs "x" (linearT numT) (Plus (Ident "x") (Ident "x"))) (Num 1)) "Expected substructural typing error" "Variable x not found."
testLinearApp5 = runTCFailureTest (App (Abs "x" (linearT numT) (Plus (Ident "y") (Ident "y"))) (Num 1)) "Expected no matching declarations found" "Variable y not found."
testLinearApp6 = runTCFailureTest (App (Abs "x" (linearT numT) (Plus (Ident "x") (Ident "y"))) (Num 1)) "Expected no matching declarations found" "Variable y not found."

testLinearApp7 = runTCSuccessTest (App (App (Abs "x" (funT (linearT numT) numT) (Ident "x")) (Abs "x" (linearT numT) (Ident "x"))) (Num 0)) numT
testLinearApp8 = runTCSuccessTest (App (Abs "x" (funT (linearT numT) numT) (App (Ident "x") (Num 0))) (Abs "x" (linearT numT) (Ident "x"))) numT
testLinearApp9 = runTCFailureTest (App (Abs "x" (funT (linearT numT) numT) (Let "x" (linearT numT) (Num 0) (App (Ident "x") (Ident "x")))) (Abs "x" (linearT numT) (Ident "x"))) "Expected substructural typing error" "Variable x not found."
testLinearApp10 = runTCSuccessTest (App (Abs "x" (funT (linearT numT) numT) (Let "y" (linearT numT) (Num 0) (App (Ident "x") (Ident "y")))) (Abs "x" (linearT numT) (Ident "x"))) numT

-- Let
testLinearLet1 = runTCSuccessTest (Let "x" (linearT numT) (Num 0) (Let "x" (linearT numT) (Ident "x") (Ident "x"))) numT
testLinearLet2 = runTCFailureTest (Let "x" (linearT numT) (Num 0) (Let "x" (linearT numT) (Num 1) (Ident "x"))) "Expected substructural typing error" "Let linear variable x no used."
testLinearLet3 = runTCFailureTest (Let "x" (linearT numT) (Num 0) (Let "x" (linearT numT) (Num 1) (Plus (Ident "x") (Ident "x")))) "Expected substructural typing error" "Variable x not found."
testLinearLet4 = runTCFailureTest (Let "x" (linearT numT) (Num 0) (Let "x" (linearT numT) (Ident "x") (Plus (Ident "x") (Ident "x")))) "Expected substructural typing error" "Variable x not found."
testLinearLet5 = runTCFailureTest (Let "x" (linearT numT) (Num 0) (Let "x" (linearT numT) (Ident "x") (Num 1))) "Expected substructural typing error" "Let linear variable x no used."

testLinearLet6 = runTCSuccessTest (Let "x" (linearT (funT (linearT numT) numT)) (Abs "x" (linearT numT) (Ident "x")) (Let "x" (linearT numT) (App (Ident "x") (Num 0)) (Ident "x"))) numT
testLinearLet7 = runTCFailureTest (Let "x" (linearT (funT (linearT numT) numT)) (Abs "x" (linearT numT) (Ident "x")) (Let "y" (linearT numT) (App (Ident "x") (Ident "y")) (Ident "x"))) "Expected no matching declarations found" "Variable y not found."
testLinearLet8 = runTCSuccessTest (Let "x" (linearT (funT (linearT numT) numT)) (Abs "x" (linearT numT) (Ident "x")) (Let "y" (linearT numT) (App (Ident "x") (Num 0)) (Ident "y"))) numT

-- Complex
testLinearComplex1 = runTCFailureTest (Let "x" (linearT numT) (Num 0) (Let "f" (linearT (funT (linearT numT) numT)) (Abs "x" (linearT numT) (Plus (Ident "x") (Ident "x"))) (App (Ident "f") (Num 1)))) "Expected substructural typing error" "Variable x not found."
testLinearComplex2 = runTCFailureTest (Let "f" (linearT (funT (linearT numT) numT)) (Abs "x" (linearT numT) (Let "x" (linearT numT) (Num 0) (Plus (Ident "x") (Num 1)))) (App (Ident "f") (Num 2))) "Expected substructural typing error" "Lambda linear variable x not used."

-- Errors
testLinearError1 = runTCFailureUnificationTest (Plus (Num 0) (Abs "x" (linearT numT) (Ident "x")))
testLinearError2 = runTCFailureUnificationTest (Plus (Abs "x" (linearT numT) (Ident "x")) (Num 0))
testLinearError3 = runTCFailureUnificationTest (Plus (Abs "x" (linearT numT) (Ident "x")) (Abs "x" (linearT numT) (Ident "x")))

testLinearError4 = runTCFailureUnificationTest (App (Abs "x" (linearT numT) (Ident "x")) (Abs "x" (linearT numT) (Ident "x")))
testLinearError5 = runTCFailureUnificationTest (App (Num 0) (Num 1))

testLinearError6 = runTCFailureUnificationTest (Let "x" (linearT numT) (Abs "x" (linearT numT) (Ident "x")) (Ident "x"))
testLinearError7 = runTCFailureUnificationTest (Let "x" (linearT (funT (linearT numT) numT)) (Num 0) (App (Ident "x") (Num 1)))

linearTests :: Test
linearTests = TestList
    [ "testLinearAbs1" ~: testLinearAbs1 
    , "testLinearAbs2" ~: testLinearAbs2 
    , "testLinearAbs3" ~: testLinearAbs3 
    , "testLinearAbs4" ~: testLinearAbs4 
    , "testLinearAbs5" ~: testLinearAbs5 
    , "testLinearAbs6" ~: testLinearAbs6 
    , "testLinearAbs7" ~: testLinearAbs7 
    , "testLinearAbs8" ~: testLinearAbs8 
    , "testLinearAbs9" ~: testLinearAbs9 
    , "testLinearAbs10" ~: testLinearAbs10 
    , "testLinearApp1" ~: testLinearApp1 
    , "testLinearApp2" ~: testLinearApp2 
    , "testLinearApp3" ~: testLinearApp3 
    , "testLinearApp4" ~: testLinearApp4 
    , "testLinearApp5" ~: testLinearApp5 
    , "testLinearApp6" ~: testLinearApp6 
    , "testLinearApp7" ~: testLinearApp7 
    , "testLinearApp8" ~: testLinearApp8 
    , "testLinearApp9" ~: testLinearApp9 
    , "testLinearApp10" ~: testLinearApp10 
    , "testLinearLet1" ~: testLinearLet1 
    , "testLinearLet2" ~: testLinearLet2 
    , "testLinearLet3" ~: testLinearLet3 
    , "testLinearLet4" ~: testLinearLet4 
    , "testLinearLet5" ~: testLinearLet5 
    , "testLinearLet6" ~: testLinearLet6 
    , "testLinearLet7" ~: testLinearLet7 
    , "testLinearLet8" ~: testLinearLet8 
    , "testLinearComplex1" ~: testLinearComplex1 
    , "testLinearComplex2" ~: testLinearComplex2 
    , "testLinearError1" ~: testLinearError1 
    , "testLinearError2" ~: testLinearError2 
    , "testLinearError3" ~: testLinearError3 
    , "testLinearError4" ~: testLinearError4 
    , "testLinearError5" ~: testLinearError5 
    , "testLinearError6" ~: testLinearError6 
    , "testLinearError7" ~: testLinearError7 
    ]

-- Affine typing rules
-- Abs
testAffineAbs1 = runTCSuccessTest (Abs "x" (affineT numT) (Num 0)) (funT numT numT)
testAffineAbs2 = runTCSuccessTest (Abs "x" (affineT numT) (Ident "x")) (funT numT numT)
testAffineAbs3 = runTCFailureTest (Abs "x" (affineT numT) (Ident "y")) "Expected no matching declarations found" "Variable y not found."
testAffineAbs4 = runTCFailureTest (Abs "x" (affineT numT) (Plus (Ident "x") (Ident "x"))) "Expected substructural typing error" "Variable x not found."
testAffineAbs5 = runTCFailureTest (Abs "x" (affineT numT) (Plus (Ident "y") (Ident "y"))) "Expected no matching declarations found" "Variable y not found."
testAffineAbs6 = runTCFailureTest (Abs "x" (affineT numT) (Plus (Ident "x") (Ident "y"))) "Expected no matching declarations found" "Variable y not found."

testAffineAbs7 = runTCSuccessTest (Abs "x" (funT (affineT numT) numT) (Ident "x")) (funT (funT (affineT numT) numT) (funT (affineT numT) numT))
testAffineAbs8 = runTCSuccessTest (Abs "x" (funT (affineT numT) numT) (App (Ident "x") (Num 0))) (funT (funT (affineT numT) numT) numT)
testAffineAbs9 = runTCFailureTest (Abs "x" (funT (affineT numT) numT) (Let "x" (affineT numT) (Num 0) (App (Ident "x") (Ident "x")))) "Expected substructural typing error" "Variable x not found."
testAffineAbs10 = runTCSuccessTest (Abs "x" (funT (affineT numT) numT) (Let "y" (affineT numT) (Num 0) (App (Ident "x") (Ident "y")))) (funT (funT (affineT numT) numT) numT)

-- App
testAffineApp1 = runTCSuccessTest (App (Abs "x" (affineT numT) (Num 0)) (Num 1)) numT
testAffineApp2 = runTCSuccessTest (App (Abs "x" (affineT numT) (Ident "x")) (Num 1)) numT
testAffineApp3 = runTCFailureTest (App (Abs "x" (affineT numT) (Ident "y")) (Num 1)) "Expected no matching declarations found" "Variable y not found."
testAffineApp4 = runTCFailureTest (App (Abs "x" (affineT numT) (Plus (Ident "x") (Ident "x"))) (Num 1)) "Expected substructural typing error" "Variable x not found."
testAffineApp5 = runTCFailureTest (App (Abs "x" (affineT numT) (Plus (Ident "y") (Ident "y"))) (Num 1)) "Expected no matching declarations found" "Variable y not found."
testAffineApp6 = runTCFailureTest (App (Abs "x" (affineT numT) (Plus (Ident "x") (Ident "y"))) (Num 1)) "Expected no matching declarations found" "Variable y not found."

testAffineApp7 = runTCSuccessTest (App (App (Abs "x" (funT (affineT numT) numT) (Ident "x")) (Abs "x" (affineT numT) (Ident "x"))) (Num 0)) numT
testAffineApp8 = runTCSuccessTest (App (Abs "x" (funT (affineT numT) numT) (App (Ident "x") (Num 0))) (Abs "x" (affineT numT) (Ident "x"))) numT
testAffineApp9 = runTCFailureTest (App (Abs "x" (funT (affineT numT) numT) (Let "x" (affineT numT) (Num 0) (App (Ident "x") (Ident "x")))) (Abs "x" (affineT numT) (Ident "x"))) "Expected substructural typing error" "Variable x not found."
testAffineApp10 = runTCSuccessTest (App (Abs "x" (funT (affineT numT) numT) (Let "y" (affineT numT) (Num 0) (App (Ident "x") (Ident "y")))) (Abs "x" (affineT numT) (Ident "x"))) numT

-- Let
testAffineLet1 = runTCSuccessTest (Let "x" (affineT numT) (Num 0) (Let "x" (affineT numT) (Ident "x") (Ident "x"))) numT
testAffineLet2 = runTCSuccessTest (Let "x" (affineT numT) (Num 0) (Let "x" (affineT numT) (Num 1) (Ident "x"))) numT
testAffineLet3 = runTCFailureTest (Let "x" (affineT numT) (Num 0) (Let "x" (affineT numT) (Num 1) (Plus (Ident "x") (Ident "x")))) "Expected substructural typing error" "Variable x not found."
testAffineLet4 = runTCFailureTest (Let "x" (affineT numT) (Num 0) (Let "x" (affineT numT) (Ident "x") (Plus (Ident "x") (Ident "x")))) "Expected substructural typing error" "Variable x not found."
testAffineLet5 = runTCSuccessTest (Let "x" (affineT numT) (Num 0) (Let "x" (affineT numT) (Ident "x") (Num 1))) numT

testAffineLet6 = runTCSuccessTest (Let "x" (affineT (funT (affineT numT) numT)) (Abs "x" (affineT numT) (Ident "x")) (Let "x" (affineT numT) (App (Ident "x") (Num 0)) (Ident "x"))) numT
testAffineLet7 = runTCFailureTest (Let "x" (affineT (funT (affineT numT) numT)) (Abs "x" (affineT numT) (Ident "x")) (Let "y" (affineT numT) (App (Ident "x") (Ident "y")) (Ident "x"))) "Expected no matching declarations found" "Variable y not found."
testAffineLet8 = runTCSuccessTest (Let "x" (affineT (funT (affineT numT) numT)) (Abs "x" (affineT numT) (Ident "x")) (Let "y" (affineT numT) (App (Ident "x") (Num 0)) (Ident "y"))) numT

-- Complex
testAffineComplex1 = runTCFailureTest (Let "x" (affineT numT) (Num 0) (Let "f" (affineT (funT (affineT numT) numT)) (Abs "x" (affineT numT) (Plus (Ident "x") (Ident "x"))) (App (Ident "f") (Num 1)))) "Expected substructural typing error" "Variable x not found."
testAffineComplex2 = runTCSuccessTest (Let "f" (affineT (funT (affineT numT) numT)) (Abs "x" (affineT numT) (Let "x" (affineT numT) (Num 0) (Plus (Ident "x") (Num 1)))) (App (Ident "f") (Num 2))) numT

-- Errors
testAffineError1 = runTCFailureUnificationTest (Plus (Num 0) (Abs "x" (affineT numT) (Ident "x")))
testAffineError2 = runTCFailureUnificationTest (Plus (Abs "x" (affineT numT) (Ident "x")) (Num 0))
testAffineError3 = runTCFailureUnificationTest (Plus (Abs "x" (affineT numT) (Ident "x")) (Abs "x" (affineT numT) (Ident "x")))

testAffineError4 = runTCFailureUnificationTest (App (Abs "x" (affineT numT) (Ident "x")) (Abs "x" (affineT numT) (Ident "x")))
testAffineError5 = runTCFailureUnificationTest (App (Num 0) (Num 1))

testAffineError6 = runTCFailureUnificationTest (Let "x" (affineT numT) (Abs "x" (affineT numT) (Ident "x")) (Ident "x"))
testAffineError7 = runTCFailureUnificationTest (Let "x" (affineT (funT (affineT numT) numT)) (Num 0) (App (Ident "x") (Num 1)))
  
affineTests :: Test
affineTests = TestList
    [ "testAffineAbs1" ~: testAffineAbs1 
    , "testAffineAbs2" ~: testAffineAbs2 
    , "testAffineAbs3" ~: testAffineAbs3 
    , "testAffineAbs4" ~: testAffineAbs4 
    , "testAffineAbs5" ~: testAffineAbs5 
    , "testAffineAbs6" ~: testAffineAbs6 
    , "testAffineAbs7" ~: testAffineAbs7 
    , "testAffineAbs8" ~: testAffineAbs8 
    , "testAffineAbs9" ~: testAffineAbs9 
    , "testAffineAbs10" ~: testAffineAbs10 
    , "testAffineApp1" ~: testAffineApp1 
    , "testAffineApp2" ~: testAffineApp2 
    , "testAffineApp3" ~: testAffineApp3 
    , "testAffineApp4" ~: testAffineApp4 
    , "testAffineApp5" ~: testAffineApp5 
    , "testAffineApp6" ~: testAffineApp6 
    , "testLinearApp7" ~: testLinearApp7 
    , "testAffineApp8" ~: testAffineApp8 
    , "testAffineApp9" ~: testAffineApp9 
    , "testAffineApp10" ~: testAffineApp10 
    , "testAffineLet1" ~: testAffineLet1 
    , "testAffineLet2" ~: testAffineLet2 
    , "testAffineLet3" ~: testAffineLet3 
    , "testAffineLet4" ~: testAffineLet4 
    , "testAffineLet5" ~: testAffineLet5 
    , "testAffineLet6" ~: testAffineLet5 
    , "testAffineLet7" ~: testAffineLet7 
    , "testAffineLet8" ~: testAffineLet8 
    , "testAffineComplex1" ~: testAffineComplex1 
    , "testAffineComplex2" ~: testAffineComplex2 
    , "testAffineError1" ~: testAffineError1 
    , "testAffineError2" ~: testAffineError2 
    , "testAffineError3" ~: testAffineError3 
    , "testAffineError4" ~: testAffineError4 
    , "testAffineError5" ~: testAffineError5 
    , "testAffineError6" ~: testAffineError6 
    , "testAffineError7" ~: testAffineError7 
    ]

main :: IO ()
main = do
    print "Non-substructural tests"
    result <- runTestTT tests
    print result
    print "Linear tests"
    linearResult <- runTestTT linearTests
    print linearResult
    print "Affine tests"
    affineResult <- runTestTT affineTests
    print affineResult
    if errors result > 0 || failures result > 0 || errors linearResult > 0 || failures linearResult > 0 || errors affineResult > 0 || failures affineResult > 0 then Exit.exitFailure else Exit.exitSuccess
