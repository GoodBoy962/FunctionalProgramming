module Main where

import Data.Unique
import System.IO.Unsafe (unsafePerformIO)

newtype Symbol = Symbol { unSymbol :: String } deriving (Eq,Read)

instance Show Symbol where
  show (Symbol x) = show x

data TermS =
      SymS Symbol
    | LamS Symbol TermS
    | AppS TermS TermS
    deriving (Eq,Read)

instance Show TermS where
    show (SymS x)       = "sym " ++ show x
    show (LamS x term)  = "(lam " ++ (show x) ++ " $ " ++ (show term) ++ ")"
    show (AppS term1 term2) = "app (" ++ (show term1) ++ ") (" ++ (show term2) ++ ")"

data TermP = TermP TermS
            -- (3)
            | Boolean Bool
            | Iff Bool TermP TermP
            | Not TermP
            | And TermP TermP
            | Or TermP TermP
            -- (4)
            | Natural Int
            | Plus TermP TermP
            | Mult TermP TermP
            -- (4*) +10%
            | Minus TermP TermP
            | Divide TermP TermP
            -- (5*) +50%
            | Y TermP
            -- (5**) +50%
            -- mutually recursive
            -- (6)
            | Pair TermP TermP
            | Fst TermP
            | Snd TermP
            -- (7)
            | Cons TermP TermP
            | IsNil TermP
            | Head TermP
            | Tail TermP
            deriving (Eq,Show,Read)

sym x = SymS (Symbol x)
lam x t = LamS (Symbol x) t
app t1 t2 = AppS t1 t2

-- (1)
-- переименовать все переменные так, чтобы все они были разными.
alpha :: TermS -> TermS
alpha (SymS x) = SymS x
alpha (AppS term1 term2) = AppS (alpha term1) (alpha term2)
alpha (LamS x term) =
  let newSymbol = (getUniqueSymbol x) in
  -- LamS newSymbol (alpha' x newSymbol term)
  LamS newSymbol (alpha' x newSymbol (alpha term))

alpha' :: Symbol -> Symbol -> TermS -> TermS
-- alpha' = error "Impl me!"
alpha' toReplace replaceWith (SymS x)
  | x == toReplace = (SymS replaceWith)
  | otherwise = (SymS x)
alpha' toReplace replaceWith (LamS x term) =
  -- LamS x (alpha' toReplace replaceWith (alpha term))
  LamS x (alpha' toReplace replaceWith term)
alpha' toReplace replaceWith (AppS term1 term2) =
  AppS (alpha' toReplace replaceWith term1) (alpha' toReplace replaceWith term2)


getUniqueSymbol :: Symbol -> Symbol
getUniqueSymbol (Symbol x) = unsafePerformIO (getUniqueSymbol' (Symbol x))

getUniqueSymbol' :: Symbol -> IO Symbol
getUniqueSymbol' (Symbol x) = do
  unique <- newUnique
  let newSymbol = (Symbol ("x" ++ show (hashUnique unique)))
  return newSymbol

-- (1)
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

--TermS example: (lam "x" $ app (lam "x" $ sym "x") (lam "x" $ lam "y" $ app (sym "y") (lam "y" $ sym "x")))

tru = lam "t" $ lam "f" $ sym "t"
fls = lam "t" $ lam "f" $ sym "f"

toTermS :: TermP -> TermS
--pairs
-- toTermS (Fst term) = app (toTermS term) (lam "t" $ lam "f" $ sym "t")
-- toTermS (Snd term) = app (toTermS term) (lam "t" $ lam "f" $ sym "f")
-- toTermS (Pair term)
--list
-- toTermS (Cons term1 term2) =
toTermS (IsNil term) = lam "c" $ lam "n" $ sym "n"
-- toTermS (Head term) =
-- toTermS (Tail term) =
--
toTermS (Boolean b) = if b then tru else fls
-- toTermS (Iff b term1 term2) = lam "b" $ lam "x" $ lam "y" $ (((toTermS(Boolean b)) (toTermS(term1))) (toTermS(term2)))
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
