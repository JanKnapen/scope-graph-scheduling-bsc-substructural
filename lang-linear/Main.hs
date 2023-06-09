module Main where

import Syntax
import qualified HMScope as S
import qualified AlgJ as J

example :: MLy
example = Plus 
           (App 
            (Abs "f"
              (Let "y" 
                (App (Ident "f") $ Num 10)
                (App (Ident "f") $ Ident "y"))
            ) 
            (Abs "x" 
              (Plus (Ident "x") (Ident "x")))
           )
          (Num 2)
example_linear = (Ident "x")
example_linear2 = Num 10
example_linear3 = Plus (Num 10) (Num 20)
example_linear5 = Let "x" (Num 10) (Ident "x")
example_linear4 = Let "x" (Num 10) (Plus (Ident "x") (Ident "x"))
example_linear6 = Let "x" (Num 10) (Num 10)
example_linear7 = Let "x" (Num 10) (Let "y" (Num 3) (Num 10))
example_linear_lambda = App (Abs "x" (Plus (Num 2) (Ident "x"))) (Num 5)
example_linear_lambda2 = App (Abs "x" (Plus (Ident "x") (Ident "x"))) (Num 5)
example_linear_lambda_shadow = Let "x" 
                          (Num 4) 
                          (Let "y" 
                            (Num 2) 
                            (Plus
                              (App
                                (Abs "x"
                                  (Plus (Ident "x") (Num 3))
                                )
                                (Ident "y")
                              )
                              (Ident "x")
                            )
                          )
example_linear_lambda_shadow2 = (App
                                  (Abs "x"
                                    (Plus (Num 3) (Num 3))
                                  )
                                  (Num 3)
                                )
example_linear_lambda_not_used = Let "f"
                                  (Abs "x"
                                    (Ident "x")
                                  )
                                  (Num 10)

main :: IO ()
main = do
    print $ snd <$> J.runTC example_linear
    print $ snd <$> J.runTC example_linear2
    print $ snd <$> J.runTC example_linear3
    print $ snd <$> J.runTC example_linear4
    print $ snd <$> J.runTC example_linear5
    print $ snd <$> J.runTC example_linear6
    print $ snd <$> J.runTC example_linear7
    print $ snd <$> J.runTC example_linear_lambda
    print $ snd <$> J.runTC example_linear_lambda2
    print $ snd <$> J.runTC example_linear_lambda_shadow
    print $ snd <$> J.runTC example_linear_lambda_shadow2
    print $ snd <$> J.runTC example_linear_lambda_not_used