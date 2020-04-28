λσ Calculus
============

```
$ cabal repl

> eval $ EApp (ELam (EVar 0)) (EVar 0)
EVar 0

> eval $ ESub (ELam $ ESub (EVar 0) (SCons (EVar 1) SId)) (SCons (EVar 0) SId)
ELam (EVar 1)
```