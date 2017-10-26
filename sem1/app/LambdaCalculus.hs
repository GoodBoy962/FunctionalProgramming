module LambdaCalculus (alpha, beta) where

import Data.Unique
import System.IO.Unsafe (unsafePerformIO)
import Term (Symbol (..), TermS (..))

alpha :: TermS -> TermS
alpha (SymS x) = SymS x
alpha (AppS term1 term2) = AppS (alpha term1) (alpha term2)
alpha (LamS x term) =
    let newSymbol = (getUniqueSymbol x) in
    LamS newSymbol (alpha' x newSymbol (alpha term))

alpha' :: Symbol -> Symbol -> TermS -> TermS
alpha' toReplace replaceWith (SymS x)
    | x == toReplace = (SymS replaceWith)
    | otherwise = (SymS x)
alpha' toReplace replaceWith (LamS x term) =
    LamS x (alpha' toReplace replaceWith term)
alpha' toReplace replaceWith (AppS term1 term2) =
    AppS (alpha' toReplace replaceWith term1) (alpha' toReplace replaceWith term2)

getUniqueSymbol :: Symbol -> Symbol
getUniqueSymbol (Symbol x) = unsafePerformIO (getUniqueSymbol' (Symbol x))

getUniqueSymbol' :: Symbol -> IO Symbol
getUniqueSymbol' (Symbol x) = do
    unique <- newUnique
    let newSymbol = (Symbol (x ++ show (hashUnique unique)))
    return newSymbol

beta :: TermS -> Maybe TermS
beta term = let reduced = beta' term
    in if reduced == term
    then Nothing
    else Just reduced

beta' :: TermS -> TermS
beta' (SymS x)       = SymS x
beta' (LamS x term)  = LamS x (beta' term)
beta' (AppS (SymS x) term) = AppS (SymS x) (beta' term)
beta' (AppS term1 term2) =
  let modifiedTerm1 = beta' term1
  in if modifiedTerm1 == term1
    then let
      modifiedTerm2 = (beta' term2)
      in if modifiedTerm2 == term2
        then apply term1 term2
        else AppS term1 modifiedTerm2
    else AppS modifiedTerm1 term2

apply :: TermS -> TermS -> TermS
apply (LamS param term1) term2 = rename param term2 term1
apply term1 term2 = AppS term1 term2

rename :: Symbol -> TermS -> TermS -> TermS
rename param term1 term2 = rename' replace term2
    where replace = \(SymS x) -> if x == param then term1 else (SymS x)

rename' :: (TermS -> TermS) -> TermS -> TermS
rename' f (SymS x) = f (SymS x)
rename' f (LamS x term) = LamS x (rename' f term)
rename' f (AppS term1 term2) = AppS (rename' f term1) (rename' f term2)
