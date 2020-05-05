{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HMLang where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (all, intersect)
import Data.Map (Map)
import Data.Set (Set)

import Common


-- The type system is Hinley-Milner
-- or a restricted form of (Girards-Reynolds) System F 
-- All the type quantifiers are out in the front
-- Hence, we do not need to specify it
-- explicitly. 

-- Types A, B ::= K | A | A -> B 
data Type = K | TVar Nat | TArr Type Type
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
