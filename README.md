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
λ> chkE [TArr K K] (TEVar 0) (K)
NotOK

λ> chkE [] (TELam K (TEVar 0)) (TArr K K)
OK

-- Inferring Expression
λ> synT [] (TELam K (TEVar 0))
(TArr K K)
```