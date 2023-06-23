{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant flip" #-}
module AlgJ (Scheme(..), tc, runTC, generalize) where

import Data.List
import qualified Data.Map as Map
import Data.Term

import Free
import Free.Logic.Exists
import Free.Logic.Equals
import Free.Error

import Syntax
import Data.Foldable
import System.IO.Unsafe (unsafePerformIO)

type Ctx = [(String, Scheme)]

instantiate :: (Functor f, Exists Ty < f) => Scheme -> Free f Ty
instantiate (Scheme vars t) = do
  subst <- mapM
    (\i -> do y <- exists; return (i,y))
    vars
  return $ substsIn subst t

generalize :: Ctx -> Ty -> Scheme
generalize ctx t =
  let s_fvs = fv t
      ctx_fvs = concatMap (sfv . snd) ctx
      gens = s_fvs \\ ctx_fvs
   in Scheme gens t

tc :: ( Functor f
      , Exists Ty < f
      , Equals Ty < f
      , Error String < f )
     => MLy -> Ctx -> Free f (Ty, Ctx)
tc (Num _) ctx = return (numT, ctx)
tc (Plus e1 e2) ctx = do
  (t1, ctx') <- tc e1 ctx
  equals t1 numT
  (t2, ctx'') <- tc e2 ctx'
  equals t2 numT
  return (numT, ctx'')
tc (Ident x) ctx = 
  let scheme = Data.List.lookup x ctx
  in case scheme of
    (Just s) -> do
      t <- instantiate s
      case t of
        (Term "Linear" [t']) -> do
          let ctx' = filter (\(k, v) -> k /= x) ctx
          return (t', ctx')
        (Term "Affine" [t']) -> do
          let ctx' = filter (\(k, v) -> k /= x) ctx
          return (t', ctx')
        (Term _ _) -> return (t, ctx)
    Nothing  -> err $ "Variable " ++ x ++ " not found."
tc (App e1 e2) ctx = do
  (t1, ctx') <- tc e1 ctx
  (t2, ctx'') <- tc e2 ctx'
  case t1 of
    (Term "->" [ti, to]) -> case ti of
      (Term "Linear" [t']) -> do
        equals t' t2
        return (to, ctx'')
      (Term "Affine" [t']) -> do
        equals t' t2
        return (to, ctx'')
      _ -> do
        equals ti t2
        return (to, ctx'')
    _ -> err "Unification error"
tc (Abs x t e) ctx = do
  case t of
    (Term "Linear" [t']) -> do
      let ctx_no_x = filter (\(k, v) -> k /= x) ctx
      let ctx_x = filter (\(k, v) -> k == x) ctx
      (t2, ctx_no_x') <- tc e $ (x, Scheme [] t) : ctx_no_x
      let scheme = lookup x ctx_no_x'
      case scheme of
        (Just s) -> err $ "Lambda linear variable " ++ x ++ " not used."
        Nothing -> do
          let ctx' = ctx_no_x' ++ ctx_x
          return $ (funT t' t2, ctx')
    (Term "Affine" [t']) -> do
      let ctx_no_x = filter (\(k, v) -> k /= x) ctx
      let ctx_x = filter (\(k, v) -> k == x) ctx
      let ctx' = (x, Scheme [] t) : ctx_no_x
      (t2, ctx'') <- tc e ctx'
      let ctx''_no_x = filter (\(k, v) -> k /= x) ctx''
      let ctx''' = ctx''_no_x ++ ctx_x
      return $ (funT t' t2, ctx''')
    (Term _ _) -> do
      (t2, ctx') <- tc e $ (x, Scheme [] t) : ctx
      return $ (funT t t2, ctx')
tc (Let x t e1 e2) ctx = do
  (t', ctx') <- tc e1 ctx
  case t of
    (Term "Linear" [t'']) -> do
      equals t' t''
      let ctx'_no_x = filter (\(k, v) -> k /= x) ctx'
      let ctx'_x = filter (\(k, v) -> k == x) ctx'
      let s = generalize ctx'_no_x t
      (t''', ctx'_no_x') <- tc e2 ((x, s) : ctx'_no_x)
      let scheme = lookup x ctx'_no_x'
      case scheme of
        (Just s) -> err $ "Let linear variable " ++ x ++ " no used."
        Nothing -> do
          let ctx'' = ctx'_no_x' ++ ctx'_x
          return (t''', ctx'')
    (Term "Affine" [t'']) -> do
      equals t' t''
      let ctx'_no_x = filter (\(k, v) -> k /= x) ctx'
      let ctx'_x = filter (\(k, v) -> k == x) ctx'
      let s = generalize ctx'_no_x t
      let ctx'' = (x, s) : ctx'_no_x
      (t''', ctx''') <- tc e2 (ctx'')
      let ctx'''_no_x = filter (\(k, v) -> k /= x) ctx'''
      let ctx'' = ctx'''_no_x ++ ctx'_x
      return (t''', ctx'')
    (Term _ _) -> do
      equals t t'
      let s = generalize ctx' t
      (t'', ctx'') <- tc e2 ((x, s) : ctx')
      return (t'', ctx'')

-- Running the type checker
runTC :: MLy -> Either String (UMap Int, Scheme)
runTC e =
  let x = un
        $ handle hErr
        $ flip (handle_ hExists) 1
        $ flip (handle_ hEquals) Map.empty
          ( tc e [] :: Free ( Equals Ty 
                            + Exists Ty
                            + Error String
                            + Nop) 
                            (Ty, Ctx)
          )
  in case x of
    Left s -> Left s
    Right (Left (UnificationError t1 t2)) -> Left $ "Unification error: " ++ show t1 ++ " != " ++ show t2
    Right (Right ((t, _), u)) -> Right (u, generalize [] $ explicate u t)
