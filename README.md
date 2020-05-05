λσ Calculus
============

```
$ cabal repl

-- Untyped evaluation
λ> eval $ EApp (ELam (EVar 0)) (EVar 0)
EVar 0

λ> eval $ ESub (ELam $ ESub (EVar 0) (SCons (EVar 1) SId)) (SCons (EVar 0) SId)
ELam (EVar 1)

-- Type checking typed Lambda
λ> chkExp [TVar 1] (TEVar 0) (TVar 0)
NotOK

λ> chkExp [] (TELam (TVar 1) (TEVar 0)) (TArr (TVar 1) (TVar 1))
OK

-- Inferring Expression
λ> inferExp (TELam (TVar 1) (TEVar 0))
(TArr (TVar 1) (TVar 1))
```