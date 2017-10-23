module BetaReduction where

import Term (Symbol (..), TermS (..))

-- (1.2)
-- один шаг редукции, если это возможно. Стратегия вычислений - полная, т.е. редуцируются все возможные редексы.
beta :: TermS -> Maybe TermS
beta term = let reduced = beta' term
    in if reduced == term
    then Nothing
    else Just reduced

beta' :: TermS -> TermS
beta' (SymS x)       = SymS x
beta' (LamS x term)  = LamS x (beta' term)
-- beta' (AppS term1 term2) = apply (beta' term1) (beta' term2) -- полная редукция
beta' (AppS (SymS x) term) = AppS (SymS x) (beta' term)
beta' (AppS (LamS x term1) term2) = apply (beta' (LamS x term1)) (beta' term2)
beta' (AppS term1 term2) = AppS (beta' term1) (beta' term2)

apply :: TermS -> TermS -> TermS
apply (LamS param term1) term2 = beta' $ rename param term2 term1
apply term1 term2 = AppS term1 term2

rename :: Symbol -> TermS -> TermS -> TermS
rename param term1 term2 = rename' replace term2
    where replace = \(SymS x) -> if x == param then term1 else (SymS x)

rename' :: (TermS -> TermS) -> TermS -> TermS
rename' f (SymS x) = f (SymS x)
rename' f (LamS x term) = LamS x (rename' f term)
rename' f (AppS term1 term2) = AppS (rename' f term1) (rename' f term2)
