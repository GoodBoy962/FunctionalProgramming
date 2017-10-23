module Main where

import Term (Symbol (..), TermS (..), TermP (..), sym, lam, app)
import AlphaConversion (alpha)
import BetaReduction (beta)

-- (2)
-- Custom constructors
-- (2.1)
-- bool
tru = lam "t" $ lam "f" $ sym "t"
fls = lam "t" $ lam "f" $ sym "f"
-- iff b x y = lam b $ lam x $ lam y $ app (app (b) (x)) (y)
-- not' b x y = lam b $ lam x $ lam y $ app (app (b) (y)) (x)
-- or' b c t f = lam b $ lam c $ lam t $ lam f $ app (app (b) (t)) (app (app (c) (t)) (f))
-- and' b c t f = lam b $ lam c $ lam t $ lam f $ app (app (b) (app (app (c) (t)) (f)) (f))) (f)
-- (2.2)
-- list
-- cons
-- isnil
-- head'
-- tail'

toTermS :: TermP -> TermS
-- (2.1) list
-- toTermS (Cons term1 term2) =
toTermS (IsNil term) = lam "c" $ lam "n" $ sym "n"
-- toTermS (Head term) =
-- toTermS (Tail term) =
-- (2.2) bool
toTermS (Boolean b) = if b then tru else fls
-- toTermS (Not term) =
-- toTermS (And term1 term2) =
-- toTermS (Or term1 term2)
--
toTermS (TermP term) = term

-- solve :: TermP -> TermS
-- solve :: TermP -> Maybe TermS
-- solve term = beta $ alpha $ toTermS $ term

main :: IO ()
main = do
  print "sem1"
  -- s <- readLnStr
  -- print $ solve s
