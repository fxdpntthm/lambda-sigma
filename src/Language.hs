{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Language where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (all, intersect)
import Data.Map (Map)
import Data.Set (Set)

import Common

-- Expressions in untyped λσ in DeBrjuin notation
data Expr {- a, b -}
     = EVar Nat           -- 1, 2 ... 
     | ELam Expr          -- (λ λ 1 0) ... 
     | EApp Expr Expr     -- (λ λ 0 1)(λ 0) ...
     | ESub Expr Subst    -- a[s]
     deriving (Show, Eq)

-- Substitutions
data Subst {- s, t -}
     = SId                -- id
     | SUp                -- ↑
     | SCons Expr  Subst  -- a ⋅ s
     | SComp Subst Subst  -- s ￮ t
     deriving (Show, Eq)

instance Lang Expr where
  -- evaluate an expression to its value
  eval :: Expr -> Expr
  -- Lookup variable
  eval v@(EVar n)  = v
  -- Lambda is a value
  eval l@(ELam _) = l

  -- Appliction is function application
  -- AKA β rule: a' b  = (λ a) b = a[b ⋅ id]
  eval (EApp fn arg) = case (eval fn) of
    ELam a -> eval (ESub a (SCons arg SId)) 
    _      -> error $ "Cannot apply " ++ show fn ++ " to " ++ show arg
  
  -- simplify e[s] 
  eval (ESub e s)  = case (eval e, s) of
    -- e[id] = e 
    (e', SId)            -> eval e'
  
    -- n[↑] = n+1
    -- n[s] = s(n) 
    (EVar n, s)          -> eval $ seval s !! (n::Int)
  
    -- (fn arg)[s] = (fn[s]) (arg[s]) 
    (EApp fn arg, s)     -> eval $ EApp (eval $ ESub fn s) (eval $ ESub arg s)
  
    -- (λ a)[s] = λ (a ⋅ (s ￮ ↑)) 
    (ELam a, s)          -> ELam $ (ESub a (SCons (EVar 0) (SComp s SUp)))

    -- a[s ￮ t] =  (a[s])[t]
    (e', SComp s' t')    -> eval $ ESub (ESub e' s') t'
    (e', s')             -> error $ "\nOops! Didn't expect this.\nTerm: " ++ show e' ++ "\nSubst: " ++ show s'

-- simplifies substitution and gives an infinite list of exprs
seval :: Subst -> [Expr]

-- id = {i/i}  = { 0/0, 1/1, 2/2, 3/3, ... }
seval (SId)       = fmap EVar [0 ..]

-- a ⋅ s = { a/0, 0/1, 1/2, ... }
seval (SCons e s) = e : seval s

-- ↑ =  {(i+1)/i} = {1/0, 2/1, ... }
seval (SUp)       = fmap EVar [1 .. ]

-- s ￮ t = {s(i)[t]/i}
seval (SComp s t) = fmap (\ex -> eval (ESub ex t)) $ seval s

--  length of a substitution | s | = (m, n)
slen :: Subst -> (Nat, Nat)
slen SId = (0,0)
slen SUp = (0,1)
slen (SCons a s) = (m + 1, n)
  where (m,n) = slen s
slen (SComp s t) = if (p >= n)
                   then (m + p - n, q)
                   else (m, q + n - p)
  where (m,n) = slen s
        (p,q) = slen t
