module TypeCheck where

-- import Data.Functor
import Data.Regex

import Free
import Free.Scope hiding (edge, new, sink)
import qualified Free.Scope as S (edge, new, sink)
import Free.Error
import Syntax

----------------------------
-- Scope Graph Parameters --
----------------------------

data Label
  = P -- Lexical Parent Label
  | D -- Declaration
  | U -- Variable Usage
  deriving (Show, Eq)

data Symbol
  = Sym

instance Eq Symbol where
  (==) _ _ = False

data Decl
  = Decl String Type          -- Variable declaration
  | AffineDecl Sc String Type -- Affine Variable declaration
  | LinearDecl Sc String Type
  | UsageDecl Symbol
  deriving (Eq)

instance Show Decl where
  show (Decl x t) = x ++ " : " ++ show t
  show (AffineDecl s x t) = "Affine with scope (" ++ show s ++ ") " ++ x ++ " : " ++ show t
  show (LinearDecl s x t) = "Linear with scope (" ++ show s ++ ") " ++ x ++ " : " ++ show t
  show (UsageDecl _) = "Usage"

-- Scope Graph Library Convenience
edge :: Scope Sc Label Decl < f => Sc -> Label -> Sc -> Free f ()
edge = S.edge @_ @Label @Decl

new :: Scope Sc Label Decl < f => Free f Sc
new = S.new @_ @Label @Decl

sink :: Scope Sc Label Decl < f => Sc -> Label -> Decl -> Free f ()
sink = S.sink @_ @Label @Decl

-- Regular expression P*D
re :: RE Label
re = Dot (Star $ Atom P) $ Atom D

-- Path order based on length
pShortest :: PathOrder Label Decl
pShortest p1 p2 = lenRPath p1 < lenRPath p2

-- Match declaration with particular name
matchDecl :: String -> Decl -> Bool
matchDecl x (Decl x' _) = x == x'
matchDecl x (AffineDecl _ x' _) = x == x'
matchDecl x (LinearDecl _ x' _) = x == x'
matchDecl _ _ = False

------------------
-- Type Checker --
------------------

-- Function that handles each language construct
tc :: ( Functor f
      -- List of 'capabilities' of type checker
      -- No need for inference: Disable parts related to first-order unification and generalization
      -- , Exists Type < f                   -- Introduce new meta-variables
      -- , Equals Type < f                   -- First-order unification
      -- , Generalize [Int] Type < f         -- HM-style generalization
      , Error String < f                  -- Emit String errors
      , Scope Sc Label Decl < f           -- Scope graph operations
      )
   => Expr -> Sc -> Free f Type

tc (Num _) _ = return NumT
tc (Plus e1 e2) sc = do
  t1 <- tc e1 sc
  t2 <- tc e2 sc
  case (t1, t2) of
    (NumT, NumT) -> return NumT
    (t1', NumT)  -> err $ "Expected left operand of plus expression to have type 'num', got '" ++ 
                          show t1' ++ "'"
    (NumT, t2')  -> err $ "Expected right operand of plus expression to have type 'num', got '" ++ 
                          show t2' ++ "'"
    (t1', t2')   -> err $ "Expected operands of plus expression to have type 'num', got '" ++ 
                          show t1' ++ "' and '" ++
                          show t2' ++ "'"
tc (App e1 e2) sc = do
  t1 <- tc e1 sc
  t2 <- tc e2 sc
  case t1 of
    (FunT (LinearT t) t') | t == t2 -> return t'
    (FunT (AffineT t) t') | t == t2 -> return t'
    (FunT t t')           | t == t2 -> return t'
    (FunT t _)                      -> err $ "Expected argument of type '" ++ show t ++ "' got '" ++ show t2 ++ "'"
    t                               -> err $ "Expected arrow type, got '" ++ show t ++ "'"
tc (Abs x t e) s = do
  s' <- new 
  edge s' P s
  case t of
    (AffineT t') -> do
      s'' <- new
      sink s' D $ AffineDecl s'' x t'
      t'' <- tc e s'
      return $ FunT t t''
    (LinearT t') -> do
      s'' <- new
      sink s' D $ LinearDecl s'' x t'
      t'' <- tc e s'
      return $ FunT t t''
    _ -> do
      sink s' D $ Decl x t
      t' <- tc e s'
      return $ FunT t t'
tc (Ident x) s = do
  ds <- query s re pShortest (matchDecl x)
  case ds of
    []  -> err "No matching declarations found"
    [(Decl _ t)] -> return t
    [(AffineDecl s' _ t)] -> do
      sink s' U $ UsageDecl Sym
      return t
    [(LinearDecl s' _ t)] -> do
      sink s' U $ UsageDecl Sym
      return t
    _   -> err "BUG: Multiple declarations found" -- cannot happen for STLC
tc (Let x t e1 e2) s = do
  s' <- new
  edge s' P s
  t1 <- tc e1 s
  case t of
    (AffineT t1) -> do
      s'' <- new
      sink s' D $ AffineDecl s'' x t1
      t'' <- tc e2 s'
      return t''
    (LinearT t1) -> do
      s'' <- new
      sink s' D $ LinearDecl s'' x t1
      t'' <- tc e2 s'
      return t''
    t1 -> do
      sink s' D $ Decl x t1
      t' <- tc e2 s'
      return t'
    _ -> err $ "Let specified type " ++ show t ++ " != expression type " ++ show t1
tc _ _ = do
  err "Not implemented yet"

isUsageDecl :: Decl -> Bool
isUsageDecl (UsageDecl _) = True
isUsageDecl _ = False

isLinearVariable :: Graph Label Decl -> Decl -> Bool
isLinearVariable g (LinearDecl s _ _) =
  let usages = [ (l,d) | (l,d) <- sinksOf g s, l == U, isUsageDecl d ]
  in (length usages) == 1
isLinearVariable _ _ = False

isAffineVariable :: Graph Label Decl -> Decl -> Bool
isAffineVariable g (AffineDecl s _ _) =
  let usages = [ (l,d) | (l,d) <- sinksOf g s, l == U, isUsageDecl d ]
  in (length usages) <= 1
isAffineVariable _ _ = False

isLinearDecl :: Decl -> Bool
isLinearDecl (LinearDecl _ _ _) = True
isLinearDecl _ = False

isAffineDecl :: Decl -> Bool
isAffineDecl (AffineDecl _ _ _) = True
isAffineDecl _ = False

isSubstructuralScope :: Graph Label Decl -> Sc -> Bool
isSubstructuralScope g s = 
  let linearVariables = [ d | (l, d) <- sinksOf g s, l == D, isLinearDecl d ]
      affineVariables = [ d | (l, d) <- sinksOf g s, l == D, isAffineDecl d ]
  in (all (\d -> isLinearVariable g d) linearVariables) && (all (\d -> isAffineVariable g d) affineVariables)

handleSubstructuralTypes :: Either String (Type, Graph Label Decl) -> Either String (Type, Graph Label Decl)
handleSubstructuralTypes result = case result of
  Left err -> Left err
  Right (t, g) -> 
    let scopes_ = [0..(scopes g)]
    in if all (\sc -> isSubstructuralScope g sc) scopes_
      then Right (t, g)
      else Left "Substructural error"

-- Tie it all together
runTC :: Expr -> Either String (Type, Graph Label Decl)
runTC e = handleSubstructuralTypes
        $ un
        $ handle hErr
        $ handle_ hScope (tc e 0) emptyGraph
