{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Language where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (all, intersect)
import Data.Map (Map)
import Data.Set (Set)

{-

    Toy implimentation of explicit-substitution lamba calculus

    Abadi, M., L. Cardelli, P.-L. Curien, and J.-J. Levy. 1989. “Explicit Substitutions.”
    In Proceedings of the 17th ACM SIGPLAN-SIGACT Symposium on Principles of Programming Languages, 31–46. POPL ’90.
    San Francisco, California, USA: Association for Computing Machinery.
    https://doi.org/10.1145/96709.96712.

-}


-- All indices are Nats
type Nat = Int

-- simple expressions in untyped λσ in debrjuin notation
data Expr {- a, b -}
     = EVar Nat           -- 1, 2 ... 
     | ELam Expr          -- (λ λ 1 0) ... 
     | EApp Expr Expr     -- (λ λ 0 1)(λ 0) ...
     | ESub Expr Subst    -- a[s]
     deriving (Show, Eq)

data Subst {- s, t -}
     = SId                -- id
     | SUp                -- ↑
     | SCons Expr  Subst  -- a ⋅ s
     | SComp Subst Subst  -- s ￮ t
     deriving (Show, Eq)

-- evaluate an expression to its value
eval :: Expr -> Expr

-- Lookup variable
eval v@(EVar n)  = v

-- Lambda is a value
eval l@(ELam _) = l

-- Appliction is function application
-- AKA β rule: (λ a) b = a[b ⋅ id]
eval (EApp fn arg) = case (eval fn) of
  ELam a -> eval (ESub a (SCons arg SId)) 
  _      -> error $ "Cannot apply " ++ show fn ++ " to " ++ show arg

 -- simplify a[s] 
eval (ESub e s)  = case (e, s) of
  -- e[id] = e 
  (e', SId)            -> eval e'

  -- n[↑] = n+1
  -- n[s] = s(n) 
  (EVar n, s)          -> eval $ seval s !! (n::Int)

  -- (fn arg)[s] = (fn[s])(arg[s]) 
  (EApp fn arg, s)     -> eval $ EApp (eval $ ESub fn s) (eval $ ESub arg s)

  -- (λ a)[s] = λ (a ⋅ (s ￮ ↑)) 
  (ELam a, s)          -> ELam $ eval (ESub a (SCons (EVar 0) (SComp s SUp)))

  (s', e')             -> error $ "\nOops! Didn't expect this.\nTerm: " ++ show s' ++ "\nSubst: " ++ show e'

-- simplifies substitution and gives an infinite list of exprs
seval :: Subst -> [Expr]

-- id = {i/i}  = { 0/0, 1/1, 2/2, 3/3, ... }
seval (SId)       = fmap EVar [0 ..]

-- a ⋅ s = { a/0, 0/1, 1/2, ... }
seval (SCons e s) = e : seval s

-- ↑ =  = {(i+1)/i} = {1/0, 2/1, ... }
seval (SUp)       = fmap EVar [1 .. ]

-- s ￮ t = {s(i)[t]/i}
seval (SComp s t) = fmap (\ex -> eval (ESub ex t)) $ seval s


-- Some simple examples
expId = ELam (EVar 0)  
exp1 = ELam $ ELam $ EApp (EVar 2) (EVar 0)
exp2 = EApp (expId) (exp1)

-- Some more examples
