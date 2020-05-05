{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module STTypeChecker where


import STLang
import Common

-- TC machinery, monads, etc. here
type Context = [Type]

data TypeChecks = OK | NotOK
  deriving (Show, Eq)

-- Type checking/inference

-- Synthesizes a type for an expression if it can
synT :: Context -> TExpr -> Type

--    G[n] = A
-- ==================
--  G |- n:A
synT gamma v@(TEVar n)
  = if n+1 > length gamma
    then error $ "Context too small!\nContext: " ++ show gamma ++ "\nExp: " ++ show v 
    else (gamma !! (n :: Int))

--    A, G |- b:B
-- ==================
--  G |- λ A. b : A -> B
synT gamma (TELam ty b) = TArr ty retTy
  where retTy = synT (ty:gamma) b

--   G |- a: A -> B    G |- b:A
-- =================================
--         G |- a b : B
synT gamma (TEApp fn arg)
  | (TArr aTy retTy) <- fnTy
               = if (argTy == aTy)
                 then retTy
                 else (error $ "Oops! Expected argument type: " ++ show aTy
                        ++ "\ndoes not match actual type: " ++ show argTy)
  | otherwise = error $ "Oops! Expression: " ++ show fn ++ " :: " ++ show fnTy ++ " is not a function type"  
  where argTy = synT gamma arg
        fnTy = synT gamma fn

--   G' |- a: A    G |- s ▷ G'
-- =================================
--         G |- a[s] : A
synT gamma (TESub t s) = synT (synS gamma s) t


-- check if the expression has a correct type
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

--  G |- a: A -> B   G |- b: A
-- ===============================
--        G |- a b : B
chkE gamma (TEApp fn arg) expTy
  | (TArr argTy resTy) <- fnTy
     = if (resTy == expTy) then (chkE gamma arg argTy) else NotOK
  | otherwise = NotOK
    where fnTy = synT gamma fn

--  G |- s ▷ G'    G' |- a: A
-- ===========================
--        G |- a[s] : A
chkE gamma (TESub a s) expTy
  = chkE gamma' a expTy
  where gamma' = synS gamma s 

-- computes the environment G' such that  G |- s ▷ G'
-- or "s has context E' in context E"
-- Definition 4.2 (Theory S1)
synS :: Context -> TSubst -> Context

-- 
-- =============
--  G |- id ▷ G
synS gamma TSId = gamma 

-- 
-- =============
--  A , G |- ↑ ▷ G
synS gamma TSUp = tail gamma

-- G |- a : A     G |- s ▷ G' 
-- ==========================
--      G |- a:A ⋅ s ▷ G'
synS gamma (TSCons a ty s)
      = if (chkE gamma a ty == OK)
        then synS (ty:gamma) s
        else error $ "Oops! synS failed: " ++ (show a)  ++ " is not of type " ++ "show ty"

-- G |- s ▷ G''     G'' |- t ▷ G' 
-- ==========================
--      G |- s ￮ t ▷ G'
synS gamma (TSComp s t) = synS (synS gamma s) t 
