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

main :: IO ()
main = do
    print $ snd <$> J.runTC example_linear
    print $ snd <$> J.runTC example_linear2
    print $ snd <$> J.runTC example_linear3