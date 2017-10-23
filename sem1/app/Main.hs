module Main where

import Data.Unique
import System.IO.Unsafe (unsafePerformIO)
import Term (Symbol (..), TermS (..), TermP (..), sym, lam, app)

-- (1.1)
-- Rename all veriables to make their names different
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
  let newSymbol = (Symbol ("arg" ++ show (hashUnique unique)))
  return newSymbol

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
beta' (AppS (LamS param term1) term2) = apply (beta' (LamS param term1)) (beta' term2)
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

-- (2)
-- Custom constructors
-- (2.1)
-- bool
tru = lam "t" $ lam "f" $ sym "t"
fls = lam "t" $ lam "f" $ sym "f"
-- iff =
-- not' =
-- and' =
-- or' =
-- (2.2)
-- list

toTermS :: TermP -> TermS
-- list
-- toTermS (Cons term1 term2) =
toTermS (IsNil term) = lam "c" $ lam "n" $ sym "n"
-- toTermS (Head term) =
-- toTermS (Tail term) =
-- bool
toTermS (Boolean b) = if b then tru else fls
-- toTermS (Not term) =
-- toTermS (And term1 term2) =
-- toTermS (Or term1 term2)
--
toTermS (TermP term) = term

-- solve :: TermP -> TermS
-- solve = beta . alpha . toTermS

main :: IO ()
main = do
  print "sem1"
  -- s <- readLnStr
  -- print $ solve s
