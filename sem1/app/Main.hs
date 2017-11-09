module Main where

import Term (Symbol (..), TermS (..), TermP (..), TermI (..),
  sym, lam, app, tru, fls, toTermS, factorial, natural)
import LambdaCalculus (alpha, beta, full)

solve :: TermP -> Either TermI TermS
solve = Right . full id (beta.alpha) . toTermS

main :: IO ()
main = do
  s <- read <$> getLine
  print $ solve s
