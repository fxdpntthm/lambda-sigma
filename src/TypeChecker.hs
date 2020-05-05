{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TypeChecker where


import TypedLanguage
import Common

-- TC machinery, monads, etc. here
type Context = [Type]

data TypeChecks = OK | NotOK
  deriving (Show, Eq)

-- Type checking/inference

-- Synthesizes a type for an expression if it can
synT :: Context -> TExpr -> Type
synT gamma v@(TEVar n)
  = if n+1 > length gamma
    then error $ "Context too small!\nContext: " ++ show gamma ++ "\nExp: " ++ show v 
    else (gamma !! (n :: Int))

synT gamma (TELam ty b) = TArr ty retTy
  where retTy = synT (ty:gamma) b

synT _ e = error $ "Oops! cannot synthesize type for: " ++ show e

chkE :: Context -> TExpr -> Type -> TypeChecks

-- =======================
-- A, G |- 1 : A  
chkE gamma v@(TEVar n) expTy = if ((synT gamma v) == expTy) 
                               then OK
                               else NotOK

--      A, G |- b: B
-- =======================
-- G |- λ A. b : A -> B
chkE gamma (TELam ty b) expTyp@(TArr argTy retTy)
  = if (ty == argTy) && (OK == chkE (ty:gamma) b retTy)
    then OK
    else NotOK

--  A, G |- a: A -> B   G |- b: A
-- ===============================
--        G |- a b : B
chkE gamma (TEApp fn arg) expTy
  | (TArr argTy resTy) <- synT (argTy:gamma) fn
     = if (resTy == expTy) then OK else NotOK
  where
    argTy = synT gamma arg 


--  G |- s ▷ G'    G' |- a: A
-- ===========================
--        G |- a[s] : A
chkE gamma (TESub a s)  expTy
  = chkE gamma' a expTy
  where gamma' = synS gamma s 

chkE _ _ _ = NotOK

-- computes the environment G' such that  G |- s ▷ G'
synS :: Context -> TSubst -> Context
synS gamma sub
  | TSId <- sub = gamma 
  | TSUp <- sub = undefined
  | (TSCons a ty s) <- sub
      = if (chkE gamma a ty == OK)
        then synS gamma s
        else error $ "Oops! synS failed: " ++ (show a)  ++ " is not of type " ++ "show ty"
  | (TSComp s t) <- sub = undefined
  | otherwise = error $ "Oops! synS failed: " ++ (show gamma) ++ " " ++ (show sub) 
