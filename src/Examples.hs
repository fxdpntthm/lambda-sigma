module Examples where

import Common
import UTLang
import STLang
import STTypeChecker

-- Some simple examples for untyped terms
expId = ELam (EVar 0)  
exp1 = ELam $ ELam $ EApp (EVar 2) (EVar 0)
exp2 = EApp (expId) (exp1)

-- Some more examples
exp3 =  EApp (ELam $ EApp (ELam $ EVar 0)
                          (EApp (ELam $ EVar 0)
                                (EVar 0)))
             (EApp (ELam $ EVar 0) (EVar 0)) 



-- some simple examples for typed terms
texpId = TELam K (TEVar 0)
texp1 = TEApp texpId (TEVar 1) 
