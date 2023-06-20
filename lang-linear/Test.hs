module Main where

import Test.HUnit

import Syntax
import qualified AlgJ as J
import qualified System.Exit as Exit
import Free.Logic.Equals

runTCTest :: MLy -> IO (Either String (UMap Int, Scheme))
runTCTest = return . J.runTC

-- Define your test cases like the following
testLetPlus :: IO ()
testLetPlus = case J.runTC (Let "x" numT (Num 10) (Plus (Ident "x") (Ident "x"))) of
  Right (_, t) -> do
    case t of
      numT -> return ()
      _ -> assertFailure "Incorrect type"
  Left e -> assertFailure "Expected type"

testLetLambdaLinearError :: IO ()
testLetLambdaLinearError = case J.runTC (Let "x" (linearT numT) (Num 10) (Plus (Ident "x") (Ident "x"))) of
  Right (_, _) -> assertFailure "Expected linear typing error"
  Left _ -> return ()

tests :: Test
tests = TestList
    -- Add your test cases to this list
    [ "testLetPlus" ~: testLetPlus
    , "testLetLambdaLinearError" ~: testLetLambdaLinearError 
    ]

main :: IO ()
main = do
    result <- runTestTT tests
    print result
    if errors result > 0 || failures result > 0 then Exit.exitFailure else Exit.exitSuccess
