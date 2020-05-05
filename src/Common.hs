module Common where

-- Some common definitions

-- All indices are Nats
type Nat = Int


-- If a is a language better evaluate
class Lang a where
  eval :: a -> a
  eval = id -- even if it is useless ¯\_(ツ)_/¯
