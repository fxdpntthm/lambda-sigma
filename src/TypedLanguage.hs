{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TypedLanguage where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (all, intersect)
import Data.Map (Map)
import Data.Set (Set)

import Common

import Language

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
  eval (TEApp te te')  = undefined
  eval (TESub te ts) = undefined  

-- Interpretation of typed λσ calculus in untyped λσ calculus
interp :: TExpr -> Expr
interp (TEVar n)        = EVar n      
interp (TELam _ te)     = ELam $ interp te
interp (TEApp te te')   = EApp (interp te) (interp te')      
interp (TESub te ts)    = ESub (interp te) (sinterp ts) 

-- interpretation of typed substitutions gives untyped λσ calculus substitutions
sinterp :: TSubst -> Subst
sinterp TSId              = SId   
sinterp TSUp              = SUp
sinterp (TSCons te _ ts)  = SCons (interp te) (sinterp ts)  
sinterp (TSComp te te')   = SComp (sinterp te) (sinterp te')

