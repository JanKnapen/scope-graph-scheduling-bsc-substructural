module Main where

import Test.HUnit

import Syntax
import TypeCheck (runTC, Label, Decl)
import qualified System.Exit as Exit
import Free.Scope (Graph)

import System.IO

runTCTest :: Expr -> IO (Either String (Type, Graph Label Decl))
runTCTest = return . runTC

runTCSuccessTest :: Expr -> Type -> IO ()
runTCSuccessTest e t = do
  result <- runTCTest e
  case result of
    Right (t', _) -> assertEqual "Incorrect type" t t'
    Left e -> assertFailure $ "Expected type " ++ show t

runTCFailureTest :: Expr -> String -> String -> IO ()
runTCFailureTest expression failureMessage expectedErrorMessage = do
  result <- runTCTest expression
  case result of
    Right (_, _) -> assertFailure failureMessage
    Left errorMessage
      | errorMessage == expectedErrorMessage -> return ()
      | otherwise -> assertFailure $ "Actual error message: \"" ++ errorMessage ++ "\" does not match with expected error message: \"" ++ expectedErrorMessage ++ "\""

-- Non-substructural typing rules
-- Base
testBase1 = runTCSuccessTest (Num 0) NumT
testBase2 = runTCFailureTest (Ident "x") "Expected no matching declarations found" "No matching declarations found"
testBase3 = runTCFailureTest (Plus (Ident "x") (Ident "x")) "Expected no matching declarations found" "No matching declarations found"
testBase4 = runTCSuccessTest (Plus (Num 0) (Num 1)) NumT

-- Abs
testAbs1 = runTCSuccessTest (Abs "x" NumT (Num 0)) (FunT NumT NumT)
testAbs2 = runTCSuccessTest (Abs "x" NumT (Ident "x")) (FunT NumT NumT)
testAbs3 = runTCFailureTest (Abs "x" NumT (Ident "y")) "Expected no matching declarations found" "No matching declarations found"
testAbs4 = runTCSuccessTest (Abs "x" NumT (Plus (Ident "x") (Ident "x"))) (FunT NumT NumT)
testAbs5 = runTCFailureTest (Abs "x" NumT (Plus (Ident "y") (Ident "y"))) "Expected no matching declarations found" "No matching declarations found"
testAbs6 = runTCFailureTest (Abs "x" NumT (Plus (Ident "x") (Ident "y"))) "Expected no matching declarations found" "No matching declarations found"

testAbs7 = runTCSuccessTest (Abs "x" (FunT NumT NumT) (Ident "x")) (FunT (FunT NumT NumT) (FunT NumT NumT))
testAbs8 = runTCSuccessTest (Abs "x" (FunT NumT NumT) (App (Ident "x") (Num 0))) (FunT (FunT NumT NumT) NumT)
testAbs9 = runTCFailureTest (Abs "x" (FunT NumT NumT) (Let "x" NumT (Num 0) (App (Ident "x") (Ident "x")))) "Expected Application first argument error" "Expected arrow type, got 'NumT'"
testAbs10 = runTCSuccessTest (Abs "x" (FunT NumT NumT) (Let "y" NumT (Num 0) (App (Ident "x") (Ident "y")))) (FunT (FunT NumT NumT) NumT)

-- App
testApp1 = runTCSuccessTest (App (Abs "x" NumT (Num 0)) (Num 1)) NumT
testApp2 = runTCSuccessTest (App (Abs "x" NumT (Ident "x")) (Num 1)) NumT
testApp3 = runTCFailureTest (App (Abs "x" NumT (Ident "y")) (Num 1)) "Expected no matching declarations found" "No matching declarations found"
testApp4 = runTCSuccessTest (App (Abs "x" NumT (Plus (Ident "x") (Ident "x"))) (Num 1)) NumT
testApp5 = runTCFailureTest (App (Abs "x" NumT (Plus (Ident "y") (Ident "y"))) (Num 1)) "Expected no matching declarations found" "No matching declarations found"
testApp6 = runTCFailureTest (App (Abs "x" NumT (Plus (Ident "x") (Ident "y"))) (Num 1)) "Expected no matching declarations found" "No matching declarations found"

testApp7 = runTCSuccessTest (App (App (Abs "x" (FunT NumT NumT) (Ident "x")) (Abs "x" NumT (Ident "x"))) (Num 0)) NumT
testApp8 = runTCSuccessTest (App (Abs "x" (FunT NumT NumT) (App (Ident "x") (Num 0))) (Abs "x" NumT (Ident "x"))) NumT
testApp9 = runTCFailureTest (App (Abs "x" (FunT NumT NumT) (Let "x" NumT (Num 0) (App (Ident "x") (Ident "x")))) (Abs "x" NumT (Ident "x"))) "Expected Application first argument error" "Expected arrow type, got 'NumT'"
testApp10 = runTCSuccessTest (App (Abs "x" (FunT NumT NumT) (Let "y" NumT (Num 0) (App (Ident "x") (Ident "y")))) (Abs "x" NumT (Ident "x"))) NumT

-- Let
testLet1 = runTCSuccessTest (Let "x" NumT (Num 0) (Let "x" NumT (Ident "x") (Ident "x"))) NumT
testLet2 = runTCSuccessTest (Let "x" NumT (Num 0) (Let "x" NumT (Num 1) (Ident "x"))) NumT
testLet3 = runTCSuccessTest (Let "x" NumT (Num 0) (Let "x" NumT (Num 1) (Plus (Ident "x") (Ident "x")))) NumT
testLet4 = runTCSuccessTest (Let "x" NumT (Num 0) (Let "x" NumT (Ident "x") (Plus (Ident "x") (Ident "x")))) NumT
testLet5 = runTCSuccessTest (Let "x" NumT (Num 0) (Let "x" NumT (Ident "x") (Num 1))) NumT

testLet6 = runTCSuccessTest (Let "x" (FunT NumT NumT) (Abs "x" NumT (Ident "x")) (Let "x" NumT (App (Ident "x") (Num 0)) (Ident "x"))) NumT
testLet7 = runTCFailureTest (Let "x" (FunT NumT NumT) (Abs "x" NumT (Ident "x")) (Let "y" NumT (App (Ident "x") (Ident "y")) (Ident "x"))) "Expected no matching declarations found" "No matching declarations found"
testLet8 = runTCSuccessTest (Let "x" (FunT NumT NumT) (Abs "x" NumT (Ident "x")) (Let "y" NumT (App (Ident "x") (Num 0)) (Ident "y"))) NumT

-- Complex
testComplex1 = runTCSuccessTest (Let "x" NumT (Num 0) (Let "f" (FunT NumT NumT) (Abs "x" NumT (Plus (Ident "x") (Ident "x"))) (App (Ident "f") (Num 1)))) NumT
testComplex2 = runTCSuccessTest (Let "f" (FunT NumT NumT) (Abs "x" NumT (Let "x" NumT (Num 0) (Plus (Ident "x") (Num 1)))) (App (Ident "f") (Num 2))) NumT

-- Errors
testError1 = runTCFailureTest (Plus (Num 0) (Abs "x" NumT (Ident "x"))) "Expected Plus right operand error" "Expected right operand of plus expression to have type 'NumT', got '(FunT NumT NumT)'"
testError2 = runTCFailureTest (Plus (Abs "x" NumT (Ident "x")) (Num 0)) "Expected Plus left operand error" "Expected left operand of plus expression to have type 'NumT', got '(FunT NumT NumT)'"
testError3 = runTCFailureTest (Plus (Abs "x" NumT (Ident "x")) (Abs "x" NumT (Ident "x"))) "Expected Plus both operands error" "Expected operands of plus expression to have type 'NumT', got '(FunT NumT NumT)' and '(FunT NumT NumT)'"

testError4 = runTCFailureTest (App (Abs "x" NumT (Ident "x")) (Abs "x" NumT (Ident "x"))) "Expected Abs argument error" "Expected argument of type 'NumT' got '(FunT NumT NumT)'"
testError5 = runTCFailureTest (App (Num 0) (Num 1)) "Expected App first argument error" "Expected arrow type, got 'NumT'"

testError6 = runTCFailureTest (Let "x" NumT (Abs "x" NumT (Ident "x")) (Ident "x")) "Expected Let type error" "Expected Let of type 'NumT' got '(FunT NumT NumT)'"
testError7 = runTCFailureTest (Let "x" (FunT NumT NumT) (Num 0) (App (Ident "x") (Num 1))) "Expected Let type error" "Expected Let of type '(FunT NumT NumT)' got 'NumT'"

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
testLinearAbs1 = runTCFailureTest (Abs "x" (LinearT NumT) (Num 0)) "Expected substructural typing error" "Substructural typing error"
testLinearAbs2 = runTCSuccessTest (Abs "x" (LinearT NumT) (Ident "x")) (FunT (LinearT NumT) NumT)
testLinearAbs3 = runTCFailureTest (Abs "x" (LinearT NumT) (Ident "y")) "Expected no matching declarations found" "No matching declarations found"
testLinearAbs4 = runTCFailureTest (Abs "x" (LinearT NumT) (Plus (Ident "x") (Ident "x"))) "Expected substructural typing error" "Substructural typing error"
testLinearAbs5 = runTCFailureTest (Abs "x" (LinearT NumT) (Plus (Ident "y") (Ident "y"))) "Expected no matching declarations found" "No matching declarations found"
testLinearAbs6 = runTCFailureTest (Abs "x" (LinearT NumT) (Plus (Ident "x") (Ident "y"))) "Expected no matching declarations found" "No matching declarations found"

testLinearAbs7 = runTCSuccessTest (Abs "x" (FunT (LinearT NumT) NumT) (Ident "x")) (FunT (FunT (LinearT NumT) NumT) (FunT (LinearT NumT) NumT))
testLinearAbs8 = runTCSuccessTest (Abs "x" (FunT (LinearT NumT) NumT) (App (Ident "x") (Num 0))) (FunT (FunT (LinearT NumT) NumT) NumT)
testLinearAbs9 = runTCFailureTest (Abs "x" (FunT (LinearT NumT) NumT) (Let "x" (LinearT NumT) (Num 0) (App (Ident "x") (Ident "x")))) "Expected Application first argument error" "Expected arrow type, got 'NumT'"
testLinearAbs10 = runTCSuccessTest (Abs "x" (FunT (LinearT NumT) NumT) (Let "y" (LinearT NumT) (Num 0) (App (Ident "x") (Ident "y")))) (FunT (FunT (LinearT NumT) NumT) NumT)

-- App
testLinearApp1 = runTCFailureTest (App (Abs "x" (LinearT NumT) (Num 0)) (Num 1)) "Expected substructural typing error" "Substructural typing error"
testLinearApp2 = runTCSuccessTest (App (Abs "x" (LinearT NumT) (Ident "x")) (Num 1)) NumT
testLinearApp3 = runTCFailureTest (App (Abs "x" (LinearT NumT) (Ident "y")) (Num 1)) "Expected no matching declarations found" "No matching declarations found"
testLinearApp4 = runTCFailureTest (App (Abs "x" (LinearT NumT) (Plus (Ident "x") (Ident "x"))) (Num 1)) "Expected substructural typing error" "Substructural typing error"
testLinearApp5 = runTCFailureTest (App (Abs "x" (LinearT NumT) (Plus (Ident "y") (Ident "y"))) (Num 1)) "Expected no matching declarations found" "No matching declarations found"
testLinearApp6 = runTCFailureTest (App (Abs "x" (LinearT NumT) (Plus (Ident "x") (Ident "y"))) (Num 1)) "Expected no matching declarations found" "No matching declarations found"

testLinearApp7 = runTCSuccessTest (App (App (Abs "x" (FunT (LinearT NumT) NumT) (Ident "x")) (Abs "x" (LinearT NumT) (Ident "x"))) (Num 0)) NumT
testLinearApp8 = runTCSuccessTest (App (Abs "x" (FunT (LinearT NumT) NumT) (App (Ident "x") (Num 0))) (Abs "x" (LinearT NumT) (Ident "x"))) NumT
testLinearApp9 = runTCFailureTest (App (Abs "x" (FunT (LinearT NumT) NumT) (Let "x" (LinearT NumT) (Num 0) (App (Ident "x") (Ident "x")))) (Abs "x" (LinearT NumT) (Ident "x"))) "Expected Application first argument error" "Expected arrow type, got 'NumT'"
testLinearApp10 = runTCSuccessTest (App (Abs "x" (FunT (LinearT NumT) NumT) (Let "y" (LinearT NumT) (Num 0) (App (Ident "x") (Ident "y")))) (Abs "x" (LinearT NumT) (Ident "x"))) NumT

-- Let
testLinearLet1 = runTCSuccessTest (Let "x" (LinearT NumT) (Num 0) (Let "x" (LinearT NumT) (Ident "x") (Ident "x"))) NumT
testLinearLet2 = runTCFailureTest (Let "x" (LinearT NumT) (Num 0) (Let "x" (LinearT NumT) (Num 1) (Ident "x"))) "Expected substructural typing error" "Substructural typing error"
testLinearLet3 = runTCFailureTest (Let "x" (LinearT NumT) (Num 0) (Let "x" (LinearT NumT) (Num 1) (Plus (Ident "x") (Ident "x")))) "Expected substructural typing error" "Substructural typing error"
testLinearLet4 = runTCFailureTest (Let "x" (LinearT NumT) (Num 0) (Let "x" (LinearT NumT) (Ident "x") (Plus (Ident "x") (Ident "x")))) "Expected substructural typing error" "Substructural typing error"
testLinearLet5 = runTCFailureTest (Let "x" (LinearT NumT) (Num 0) (Let "x" (LinearT NumT) (Ident "x") (Num 1))) "Expected substructural typing error" "Substructural typing error"

testLinearLet6 = runTCSuccessTest (Let "x" (LinearT (FunT (LinearT NumT) NumT)) (Abs "x" (LinearT NumT) (Ident "x")) (Let "x" (LinearT NumT) (App (Ident "x") (Num 0)) (Ident "x"))) NumT
testLinearLet7 = runTCFailureTest (Let "x" (LinearT (FunT (LinearT NumT) NumT)) (Abs "x" (LinearT NumT) (Ident "x")) (Let "y" (LinearT NumT) (App (Ident "x") (Ident "y")) (Ident "x"))) "Expected no matching declarations found" "No matching declarations found"
testLinearLet8 = runTCSuccessTest (Let "x" (LinearT (FunT (LinearT NumT) NumT)) (Abs "x" (LinearT NumT) (Ident "x")) (Let "y" (LinearT NumT) (App (Ident "x") (Num 0)) (Ident "y"))) NumT

-- Complex
testLinearComplex1 = runTCFailureTest (Let "x" (LinearT NumT) (Num 0) (Let "f" (LinearT (FunT (LinearT NumT) NumT)) (Abs "x" (LinearT NumT) (Plus (Ident "x") (Ident "x"))) (App (Ident "f") (Num 1)))) "Expected substructural typing error" "Substructural typing error"
testLinearComplex2 = runTCFailureTest (Let "f" (LinearT (FunT (LinearT NumT) NumT)) (Abs "x" (LinearT NumT) (Let "x" (LinearT NumT) (Num 0) (Plus (Ident "x") (Num 1)))) (App (Ident "f") (Num 2))) "Expected substructural typing error" "Substructural typing error"

-- Errors
testLinearError1 = runTCFailureTest (Plus (Num 0) (Abs "x" (LinearT NumT) (Ident "x"))) "Expected Plus right operand error" "Expected right operand of plus expression to have type 'NumT', got '(FunT (LinearT NumT) NumT)'"
testLinearError2 = runTCFailureTest (Plus (Abs "x" (LinearT NumT) (Ident "x")) (Num 0)) "Expected Plus left operand error" "Expected left operand of plus expression to have type 'NumT', got '(FunT (LinearT NumT) NumT)'"
testLinearError3 = runTCFailureTest (Plus (Abs "x" (LinearT NumT) (Ident "x")) (Abs "x" (LinearT NumT) (Ident "x"))) "Expected Plus both operands error" "Expected operands of plus expression to have type 'NumT', got '(FunT (LinearT NumT) NumT)' and '(FunT (LinearT NumT) NumT)'"

testLinearError4 = runTCFailureTest (App (Abs "x" (LinearT NumT) (Ident "x")) (Abs "x" (LinearT NumT) (Ident "x"))) "Expected Abs argument error" "Expected argument of type '(LinearT NumT)' got '(FunT (LinearT NumT) NumT)'"
testLinearError5 = runTCFailureTest (App (Num 0) (Num 1)) "Expected App first argument error" "Expected arrow type, got 'NumT'"

testLinearError6 = runTCFailureTest (Let "x" (LinearT NumT) (Abs "x" (LinearT NumT) (Ident "x")) (Ident "x")) "Expected Let type error" "Expected Let of type '(LinearT NumT)' got '(FunT (LinearT NumT) NumT)'"
testLinearError7 = runTCFailureTest (Let "x" (LinearT (FunT (LinearT NumT) NumT)) (Num 0) (App (Ident "x") (Num 1))) "Expected Let type error" "Expected Let of type '(LinearT (FunT (LinearT NumT) NumT))' got 'NumT'"
  
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
testAffineAbs1 = runTCSuccessTest (Abs "x" (AffineT NumT) (Num 0)) (FunT (AffineT NumT) NumT)
testAffineAbs2 = runTCSuccessTest (Abs "x" (AffineT NumT) (Ident "x")) (FunT (AffineT NumT) NumT)
testAffineAbs3 = runTCFailureTest (Abs "x" (AffineT NumT) (Ident "y")) "Expected no matching declarations found" "No matching declarations found"
testAffineAbs4 = runTCFailureTest (Abs "x" (AffineT NumT) (Plus (Ident "x") (Ident "x"))) "Expected substructural typing error" "Substructural typing error"
testAffineAbs5 = runTCFailureTest (Abs "x" (AffineT NumT) (Plus (Ident "y") (Ident "y"))) "Expected no matching declarations found" "No matching declarations found"
testAffineAbs6 = runTCFailureTest (Abs "x" (AffineT NumT) (Plus (Ident "x") (Ident "y"))) "Expected no matching declarations found" "No matching declarations found"

testAffineAbs7 = runTCSuccessTest (Abs "x" (FunT (AffineT NumT) NumT) (Ident "x")) (FunT (FunT (AffineT NumT) NumT) (FunT (AffineT NumT) NumT))
testAffineAbs8 = runTCSuccessTest (Abs "x" (FunT (AffineT NumT) NumT) (App (Ident "x") (Num 0))) (FunT (FunT (AffineT NumT) NumT) NumT)
testAffineAbs9 = runTCFailureTest (Abs "x" (FunT (AffineT NumT) NumT) (Let "x" (AffineT NumT) (Num 0) (App (Ident "x") (Ident "x")))) "Expected Application first argument error" "Expected arrow type, got 'NumT'"
testAffineAbs10 = runTCSuccessTest (Abs "x" (FunT (AffineT NumT) NumT) (Let "y" (AffineT NumT) (Num 0) (App (Ident "x") (Ident "y")))) (FunT (FunT (AffineT NumT) NumT) NumT)

-- App
testAffineApp1 = runTCSuccessTest (App (Abs "x" (AffineT NumT) (Num 0)) (Num 1)) NumT
testAffineApp2 = runTCSuccessTest (App (Abs "x" (AffineT NumT) (Ident "x")) (Num 1)) NumT
testAffineApp3 = runTCFailureTest (App (Abs "x" (AffineT NumT) (Ident "y")) (Num 1)) "Expected no matching declarations found" "No matching declarations found"
testAffineApp4 = runTCFailureTest (App (Abs "x" (AffineT NumT) (Plus (Ident "x") (Ident "x"))) (Num 1)) "Expected substructural typing error" "Substructural typing error"
testAffineApp5 = runTCFailureTest (App (Abs "x" (AffineT NumT) (Plus (Ident "y") (Ident "y"))) (Num 1)) "Expected no matching declarations found" "No matching declarations found"
testAffineApp6 = runTCFailureTest (App (Abs "x" (AffineT NumT) (Plus (Ident "x") (Ident "y"))) (Num 1)) "Expected no matching declarations found" "No matching declarations found"

testAffineApp7 = runTCSuccessTest (App (App (Abs "x" (FunT (AffineT NumT) NumT) (Ident "x")) (Abs "x" (AffineT NumT) (Ident "x"))) (Num 0)) NumT
testAffineApp8 = runTCSuccessTest (App (Abs "x" (FunT (AffineT NumT) NumT) (App (Ident "x") (Num 0))) (Abs "x" (AffineT NumT) (Ident "x"))) NumT
testAffineApp9 = runTCFailureTest (App (Abs "x" (FunT (AffineT NumT) NumT) (Let "x" (AffineT NumT) (Num 0) (App (Ident "x") (Ident "x")))) (Abs "x" (AffineT NumT) (Ident "x"))) "Expected Application first argument error" "Expected arrow type, got 'NumT'"
testAffineApp10 = runTCSuccessTest (App (Abs "x" (FunT (AffineT NumT) NumT) (Let "y" (AffineT NumT) (Num 0) (App (Ident "x") (Ident "y")))) (Abs "x" (AffineT NumT) (Ident "x"))) NumT

-- Let
testAffineLet1 = runTCSuccessTest (Let "x" (AffineT NumT) (Num 0) (Let "x" (AffineT NumT) (Ident "x") (Ident "x"))) NumT
testAffineLet2 = runTCSuccessTest (Let "x" (AffineT NumT) (Num 0) (Let "x" (AffineT NumT) (Num 1) (Ident "x"))) NumT
testAffineLet3 = runTCFailureTest (Let "x" (AffineT NumT) (Num 0) (Let "x" (AffineT NumT) (Num 1) (Plus (Ident "x") (Ident "x")))) "Expected substructural typing error" "Substructural typing error"
testAffineLet4 = runTCFailureTest (Let "x" (AffineT NumT) (Num 0) (Let "x" (AffineT NumT) (Ident "x") (Plus (Ident "x") (Ident "x")))) "Expected substructural typing error" "Substructural typing error"
testAffineLet5 = runTCSuccessTest (Let "x" (AffineT NumT) (Num 0) (Let "x" (AffineT NumT) (Ident "x") (Num 1))) NumT

testAffineLet6 = runTCSuccessTest (Let "x" (AffineT (FunT (AffineT NumT) NumT)) (Abs "x" (AffineT NumT) (Ident "x")) (Let "x" (AffineT NumT) (App (Ident "x") (Num 0)) (Ident "x"))) NumT
testAffineLet7 = runTCFailureTest (Let "x" (AffineT (FunT (AffineT NumT) NumT)) (Abs "x" (AffineT NumT) (Ident "x")) (Let "y" (AffineT NumT) (App (Ident "x") (Ident "y")) (Ident "x"))) "Expected no matching declarations found" "No matching declarations found"
testAffineLet8 = runTCSuccessTest (Let "x" (AffineT (FunT (AffineT NumT) NumT)) (Abs "x" (AffineT NumT) (Ident "x")) (Let "y" (AffineT NumT) (App (Ident "x") (Num 0)) (Ident "y"))) NumT

-- Complex
testAffineComplex1 = runTCFailureTest (Let "x" (AffineT NumT) (Num 0) (Let "f" (AffineT (FunT (AffineT NumT) NumT)) (Abs "x" (AffineT NumT) (Plus (Ident "x") (Ident "x"))) (App (Ident "f") (Num 1)))) "Expected substructural typing error" "Substructural typing error"
testAffineComplex2 = runTCSuccessTest (Let "f" (AffineT (FunT (AffineT NumT) NumT)) (Abs "x" (AffineT NumT) (Let "x" (AffineT NumT) (Num 0) (Plus (Ident "x") (Num 1)))) (App (Ident "f") (Num 2))) NumT

-- Errors
testAffineError1 = runTCFailureTest (Plus (Num 0) (Abs "x" (AffineT NumT) (Ident "x"))) "Expected Plus right operand error" "Expected right operand of plus expression to have type 'NumT', got '(FunT (AffineT NumT) NumT)'"
testAffineError2 = runTCFailureTest (Plus (Abs "x" (AffineT NumT) (Ident "x")) (Num 0)) "Expected Plus left operand error" "Expected left operand of plus expression to have type 'NumT', got '(FunT (AffineT NumT) NumT)'"
testAffineError3 = runTCFailureTest (Plus (Abs "x" (AffineT NumT) (Ident "x")) (Abs "x" (AffineT NumT) (Ident "x"))) "Expected Plus both operands error" "Expected operands of plus expression to have type 'NumT', got '(FunT (AffineT NumT) NumT)' and '(FunT (AffineT NumT) NumT)'"

testAffineError4 = runTCFailureTest (App (Abs "x" (AffineT NumT) (Ident "x")) (Abs "x" (AffineT NumT) (Ident "x"))) "Expected Abs argument error" "Expected argument of type '(AffineT NumT)' got '(FunT (AffineT NumT) NumT)'"
testAffineError5 = runTCFailureTest (App (Num 0) (Num 1)) "Expected App first argument error" "Expected arrow type, got 'NumT'"

testAffineError6 = runTCFailureTest (Let "x" (AffineT NumT) (Abs "x" (AffineT NumT) (Ident "x")) (Ident "x")) "Expected Let type error" "Expected Let of type '(AffineT NumT)' got '(FunT (AffineT NumT) NumT)'"
testAffineError7 = runTCFailureTest (Let "x" (AffineT (FunT (AffineT NumT) NumT)) (Num 0) (App (Ident "x") (Num 1))) "Expected Let type error" "Expected Let of type '(AffineT (FunT (AffineT NumT) NumT))' got 'NumT'"
  
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
    , "testAffineApp7" ~: testAffineApp7 
    , "testAffineApp8" ~: testAffineApp8 
    , "testAffineApp9" ~: testAffineApp9 
    , "testAffineApp10" ~: testAffineApp10 
    , "testAffineLet1" ~: testAffineLet1 
    , "testAffineLet2" ~: testAffineLet2 
    , "testAffineLet3" ~: testAffineLet3 
    , "testAffineLet4" ~: testAffineLet4 
    , "testAffineLet5" ~: testAffineLet5 
    , "testAffineLet6" ~: testAffineLet6 
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
