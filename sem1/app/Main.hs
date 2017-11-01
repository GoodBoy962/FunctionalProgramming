module Main where

import Term (Symbol (..), TermS (..), TermP (..), TermI (..),
  sym, lam, app, toTermS,
  tru, fls, iff', not', or', and',
  cons', head', isNil', tail', nil',
  termP)
import LambdaCalculus (alpha, beta, full)

solve :: TermP -> Either TermI TermS
solve = Right . full alpha beta . toTermS

main :: IO ()
main = do
  s <- read <$> getLine
  print $ solve s
