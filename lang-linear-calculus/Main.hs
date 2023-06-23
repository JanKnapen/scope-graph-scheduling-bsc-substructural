module Main where

import Syntax
import qualified AlgJ as J

example :: MLy
example = (Abs "x" (funT (linearT numT) numT) (Ident "x"))

main :: IO ()
main =
  print $ snd <$> J.runTC example