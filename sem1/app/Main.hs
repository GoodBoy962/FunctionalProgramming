module Main where

import Term (Symbol (..), TermS (..), TermP (..), TermI (..),
  sym, lam, app, toTermS,
  tru, fls, iff', not', or', and',
  cons', head', isNil', tail', nil',
  termP)
import LambdaCalculus (alpha, beta)


solve :: TermP -> Either (Maybe TermI) (Maybe TermS)
solve = Right . beta . alpha . toTermS

main :: IO ()
main = do
  s <- read <$> getLine
  print $ solve s
