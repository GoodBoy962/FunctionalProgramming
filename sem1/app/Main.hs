module Main where

import Term (Symbol (..), TermS (..), TermP (..), sym, lam, app)
import AlphaConversion (alpha)
import BetaReduction (beta)

tru = lam "t" $ lam "f" $ sym "t"
fls = lam "t" $ lam "f" $ sym "f"
iff' b x y = app (app (app (lam "b" $ lam "x" $ lam "y" $ app (app (sym "b") (sym "x")) (sym "y")) (b)) (x)) (y)
not' x = lam "x" $ app (app (x) (fls)) (tru)
or' x y = lam "x" $ lam "y" $ app (app (x) (tru)) (y)
and' x y = lam "x" $ lam "y" $ app (app (x) (y)) (fls)
-- list'
-- cons'
isNil' t = app (lam "c" $ lam "n" $ sym "n") (toTermS t)
-- head'
-- tail'
termP p = TermP p

toTermS :: TermP -> TermS
-- (2.1) list
-- toTermS (Cons term1 term2) =
toTermS (IsNil term) = isNil' term
-- toTermS (Head term) =
-- toTermS (Tail term) =
-- (2.2) bool
toTermS (Boolean b) = if b then tru else fls
toTermS (Iff b term1 term2) = iff' (toTermS (Boolean (b))) (toTermS term1) (toTermS term2)
toTermS (Not term) = not' (toTermS term)
toTermS (And term1 term2) = and' (toTermS term1) (toTermS term2)
toTermS (Or term1 term2) = or' (toTermS term1) (toTermS term2)
--
toTermS (TermP term) = term

solve :: TermP -> Maybe TermS
solve = beta . alpha . toTermS

main :: IO ()
main = do
  print "sem1"
  -- s <- readLnStr
  -- print $ solve s
