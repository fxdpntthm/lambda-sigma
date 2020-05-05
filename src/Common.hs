{-# LANGUAGE MultiParamTypeClasses #-}


module Common where

-- Some common definitions

-- All indices are Nats
type Nat = Int


-- If a is a language better evaluate
class Lang a where
  eval :: a -> a
  eval = id -- even if it is useless ¯\_(ツ)_/¯


-- Interpretation of a language term in some domain
-- ⟦ . ⟧ -> D
class (Lang a) => Interpretable a d where
  interp :: a -> d
