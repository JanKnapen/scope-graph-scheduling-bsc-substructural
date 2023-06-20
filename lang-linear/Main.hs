module Main where

import Syntax
import qualified AlgJ as J

example :: MLy
-- IDENT TESTS
example = Let "x" numT (Num 10) (Plus (Ident "x") (Ident "x"))
-- example = Let "x" (linearT numT) (Num 10) (Plus (Ident "x") (Ident "x"))
-- example = Let "x" (affineT numT) (Num 10) (Plus (Ident "x") (Ident "x"))

-- LET TESTS
-- example = Let "x" (linearT numT) (Num 10) (Ident "x")
-- example = Let "x" (linearT numT) (Num 10) (Num 10)
-- example = Let "x" (affineT numT) (Num 10) (Ident "x")
-- example = Let "x" (affineT numT) (Num 10) (Num 10)
-- example = Let "x" numT (Num 10) (Num 10)

-- ABS TEST
-- example = Abs "x" numT (Num 10)
-- example = Abs "x" numT (Ident "x")
-- example = Abs "x" (linearT numT) (Num 10)
-- example = Abs "x" (linearT numT) (Ident "x")

-- APP TEST
-- example = App (Abs "x" numT (Num 10)) (Num 10)
-- example = App (Abs "x" (linearT numT) (Num 10)) (Num 10)
-- example = App (Abs "x" (linearT numT) (Ident "x")) (Num 10)
-- example = App (Abs "x" (affineT numT) (Num 10)) (Num 10)
-- example = App (Abs "x" (affineT numT) (Ident "x")) (Num 10)

example_linear = (Ident "x")
example_linear2 = Num 10
example_linear3 = Plus (Num 10) (Num 20)
example_linear5 = Let "x" numT (Num 10) (Ident "x")
example_linear4 = Let "x" numT (Num 10) (Plus (Ident "x") (Ident "x"))
example_linear6 = Let "x" numT (Num 10) (Num 10)
example_linear7 = Let "x" numT (Num 10) (Let "y" numT (Num 3) (Num 10))
example_linear_lambda = App (Abs "x" numT (Plus (Num 2) (Ident "x"))) (Num 5)
example_linear_lambda2 = App (Abs "x" numT (Plus (Ident "x") (Ident "x"))) (Num 5)
example_linear_lambda_shadow = Let "x" numT
                          (Num 4) 
                          (Let "y" numT
                            (Num 2) 
                            (Plus
                              (App
                                (Abs "x" numT
                                  (Plus (Ident "x") (Num 3))
                                )
                                (Ident "y")
                              )
                              (Ident "x")
                            )
                          )
example_linear_lambda_shadow2 = (App
                                  (Abs "x" numT
                                    (Plus (Num 3) (Num 3))
                                  )
                                  (Num 3)
                                )
example_linear_lambda_not_used = Let "f" numT
                                  (Abs "x" numT
                                    (Ident "x")
                                  )
                                  (Num 10)

main :: IO ()
main =
  -- print $ snd <$> J.runTC example
  case J.runTC example of
    Left e -> putStrLn e
    Right (_, scheme) -> do
      case scheme of
        numT -> putStrLn "Result is of type Num"
        _ -> print scheme
  -- case result of
  --   Scheme [] t -> putStrLn "Incorrect"
  --   _ -> putStrLn "Incorrect"
    -- print $ snd <$> J.runTC example
    -- print $ snd <$> J.runTC example_linear2
    -- print $ snd <$> J.runTC example_linear3
    -- print $ snd <$> J.runTC example_linear4
    -- print $ snd <$> J.runTC example_linear5
    -- print $ snd <$> J.runTC example_linear6
    -- print $ snd <$> J.runTC example_linear7
    -- print $ snd <$> J.runTC example_linear_lambda
    -- print $ snd <$> J.runTC example_linear_lambda2
    -- print $ snd <$> J.runTC example_linear_lambda_shadow
    -- print $ snd <$> J.runTC example_linear_lambda_shadow2
    -- print $ snd <$> J.runTC example_linear_lambda_not_used