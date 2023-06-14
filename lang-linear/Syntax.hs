{-# LANGUAGE FlexibleInstances #-}
module Syntax where

import Data.Term
import Data.List (intercalate, nub, (\\))

-- Language to type check
data MLy
  = Num Int
  | Plus MLy MLy
  | Abs String Ty MLy
  | Ident String
  | App MLy MLy
  | Let String Ty MLy MLy

-- Types
type Ty = Term Int

instance Show Ty where
  show (Const i) = "α" ++ show i
  show (Var i) = "α" ++ show i
  show (Term "∀" ts) = "(∀ " ++ unwords (map show (init ts)) ++ ". " ++ show (last ts) ++ ")"
  show (Term "->" [t1, t2]) = show t1 ++ " -> " ++ show t2
  show (Term "Num" []) = "Num"
  show (Term "Linear" [t]) = "Linear " ++ show t
  show (Term "Affine" [t]) = "Affine " ++ show t
  show (Term f ts) = "(" ++ f ++ unwords (map show ts) ++ ")"

-- Type construction
numT = Term "Num" []
funT s t = Term "->" [s, t]
linearT t = Term "Linear" [t]
affineT t = Term "Affine" [t]
schemeT xs t | not (null xs) = Term "∀" (map Const xs ++ [t])
             | otherwise = t

-- Free variables
fv :: Term Int -> [Int]
fv (Const _) = []
fv (Var i) = [i]
fv (Term f ts) | f /= "∀" = nub $ concatMap fv ts
               | otherwise = let bs = concatMap c2fv (init ts)
                 in nub (fv (last ts) \\ bs)
  where
    c2fv (Const i) = [i]
    c2fv _ = []

-- Type context
data Scheme = Scheme [Int] Ty

instance Show Scheme where
  show (Scheme [] t) = show t
  show (Scheme vars t) = "(∀ "
                      ++ unwords (map (\i' -> "α" ++ show i') vars) 
                      ++ ". " 
                      ++ show t
                      ++ ")"

sfv :: Scheme -> [Int]
sfv (Scheme vars t) = fv t \\ vars

