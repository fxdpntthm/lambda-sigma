{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module STLang where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (all, intersect)
import Data.Map (Map)
import Data.Set (Set)

import Common

import UTLang

-- Types A, B ::= K | A -> B 
data Type = K | TArr Type Type
  deriving (Show, Eq)

-- Environment ::= Nil | A, E
type Env = [Type]

-- Typed expressions/Terms in typed λσ using debrjuin notation
data TExpr {- a, b -}
     = TEVar Nat             -- 1, 2 ... 
     | TELam Type  TExpr     -- (λA. λB. 1 0) ... 
     | TEApp TExpr TExpr     -- (λA. λB. 0 1)(λC. 0) ...
     | TESub TExpr TSubst    -- a[s]
     deriving (Show, Eq)

data TSubst {- s, t -}
     = TSId                        -- id
     | TSUp                        -- ↑
     | TSCons TExpr Type TSubst    -- a:A ⋅ s
     | TSComp TSubst TSubst        -- s ￮ t
     deriving (Show, Eq)


--  length of a typed substitution | s | = (m, n)
tslen :: TSubst -> (Nat, Nat)
tslen TSId = (0,0)
tslen TSUp = (0,1)
tslen (TSCons a _ s) = (m + 1, n)
  where (m,n) = tslen s
tslen (TSComp s t) = if (p >= n)
                   then (m + p - n, q)
                   else (m, q + n - p)
  where (m,n) = tslen s
        (p,q) = tslen t


instance Lang TExpr where
  eval :: TExpr -> TExpr
  eval v@(TEVar _)     = v 
  eval l@(TELam _ _)   = l 
  eval (TEApp te@(TELam _ b) te') = undefined  
  eval (TESub te ts) = undefined
  eval e = error $ "Oops! Cannot evaluate" ++ show e

instance Lang TSubst where
  eval = id

instance Interpretable TExpr Expr where
  interp :: TExpr -> Expr
  interp (TEVar n)        = EVar n
  interp (TELam _ te)     = ELam $ interp te
  interp (TEApp te te')   = EApp (interp te) (interp te')      
  interp (TESub te ts)    = ESub (interp te) (interp ts) 

-- interpretation of typed substitutions gives untyped λσ calculus substitutions
-- ⟦ . ⟧ -> Subst
instance Interpretable TSubst Subst where
  interp :: TSubst -> Subst
  interp TSId              = SId   
  interp TSUp              = SUp
  interp (TSCons te _ ts)  = SCons (interp te) (interp ts)  
  interp (TSComp te te')   = SComp (interp te) (interp te')

