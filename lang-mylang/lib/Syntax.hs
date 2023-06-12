module Syntax where

data Type
  = NumT
  | FunT Type Type
  | AffineT Type
  | LinearT Type
  deriving Eq
-- To use inference, replace `Type` with
-- type Ty = Term Int
-- (Term imported from Data.Term)
-- See also `lang-hm/Syntax` for an example.

data Expr
  = Num Int
  | Plus Expr Expr
  | App Expr Expr
  | Ident String
  | Abs String Type Expr
  | Let String Type Expr Expr
  deriving (Eq, Show)

instance Show Type where
  show NumT = "num"
  show (FunT ti to) = "(" ++ show ti ++ " -> " ++ show to ++ ")"
  show (AffineT t) = "(Affine Type: " ++ show t ++ ")"
  show (LinearT t) = "(Linear Type: " ++ show t ++ ")"

example :: Expr
example = App (Abs "x" (AffineT NumT) (Plus (Ident "x") (Ident "x"))) (Num 21)
-- example :: Expr
-- example = App (Abs "x" (AffineT NumT) (Plus (Ident "x") (Num 20))) (Num 21)
