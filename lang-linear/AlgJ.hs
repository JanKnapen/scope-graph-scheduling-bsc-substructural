{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant flip" #-}
module AlgJ where

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
  let scheme = lookup x ctx
  in case scheme of
    (Just s) -> do
      t <- instantiate s
      let ctx' = filter (\(k, v) -> k /= x) ctx
      -- ctx' <- delete (x, s) ctx
      -- ctx' <- [ (k, (Scheme vars t)) | (k, (Scheme vars t)) <- ctx, k /= x ] 
      -- ctx' <- delete (x, generalize ctx t) ctx -- TODO: remove from context?
      -- let ctx' = Map.lookup x ctx
      -- return (t, ctx')
      return (t, ctx')
    Nothing  -> err $ "Variable " ++ x ++ " not found."
tc (App e1 e2) ctx = do
  (t1, ctx') <- tc e1 ctx
  (t2, ctx'') <- tc e2 ctx'
  t' <- exists
  equals t1 $ funT t2 t'
  return (t', ctx'')
tc (Abs x e) ctx = do
  t1  <- exists
  -- ctx_no_x <- ctx --TODO: remove x
  -- ctx_x <- ctx --TODO: get context with only x
  -- (t2, ctx_no_x') <- tc e $ (x, Scheme [] t1) : ctx_no_x
  (t2, ctx') <- tc e $ (x, Scheme [] t1) : ctx
  -- let scheme = loookup x ctx_no_x'
  let scheme = lookup x ctx'
  case scheme of
    (Just s) -> err $ "Lambda variable " ++ x ++ " not used."
    Nothing -> do
      -- ctx' <- ctx --TODO: concatenate ctx_no_x' and ctx_x
      return $ (funT t1 t2, ctx')
tc (Let x e1 e2) ctx = do
  (t, ctx') <- tc e1 ctx
  let s  = generalize ctx' t
  tc e2 ((x, s): ctx')

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
    Right (Right ((t, []), u)) -> Right (u, generalize [] $ explicate u t)
    Right (Right ((t, ctx'), u)) -> Left $ "Linear error, variables not used: " ++ show ctx'
