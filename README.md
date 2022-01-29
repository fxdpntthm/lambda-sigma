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

Toy implimentation of explicit-substitution lamba calculus described here:

Abadi, M., L. Cardelli, P.-L. Curien, and J.-J. Levy. 1989. _Explicit Substitutions._
In Proceedings of the 17th ACM SIGPLAN-SIGACT Symposium on Principles of Programming Languages, 31–46. POPL ’90.
San Francisco, California, USA: Association for Computing Machinery.
https://doi.org/10.1145/96709.96712.
