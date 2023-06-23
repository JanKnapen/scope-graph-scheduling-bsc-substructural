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
  show NumT = "NumT"
  show (FunT ti to) = "(FunT " ++ show ti ++ " " ++ show to ++ ")"
  show (AffineT t) = "(AffineT " ++ show t ++ ")"
  show (LinearT t) = "(LinearT " ++ show t ++ ")"

example :: Expr
-- example = App (Abs "x" (AffineT NumT) (Plus (Ident "x") (Ident "x"))) (Num 21)
-- example :: Expr
-- example = App (Abs "x" (AffineT NumT) (Plus (Ident "x") (Num 20))) (Num 21)

-- ERROR
example = (App 
              (Abs 
                "f" 
                (FunT (LinearT NumT) NumT) 
                (App (Ident "f") (Num 1)) 
              )
              (Abs 
                "x" 
                (LinearT NumT) 
                (Plus (Ident "x") (Num 2))
              ))
-- example = Let "x" (LinearT NumT) (Num 1) (
--   Let "x" (LinearT NumT) (Num 2) (Plus (Ident "x") (Ident "x"))
--   )
-- example = Let "x" (LinearT NumT) (Num 1) (
--   Let "x" (LinearT NumT) (Num 2) (Ident "x")
--   )
-- example = Let "f" (LinearT (FunT NumT NumT)) (
--   Abs "x" (LinearT NumT) (
--     Let "x" (LinearT NumT) (Num 1) (
--       Plus (Ident "x") (Ident "x")
--     )
--   )) (
--   App (Ident "f") (Num 2)
--   )

-- NO ERROR
-- example = Let "x" (LinearT NumT) (Num 1) (
--   Let "f" (LinearT (FunT NumT NumT)) 
--   (
--     Abs "x" (LinearT NumT) (Plus (Ident "x") (Num 1))
--     ) 
--     (
--       App (Ident "f") (Ident "x")
--       )
--   )