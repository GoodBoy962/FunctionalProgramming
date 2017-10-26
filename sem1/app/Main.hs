module Main where

import Term (Symbol (..), TermS (..), TermP (..), sym, lam, app, toTermS, tru, fls, iff', not', or', and', cons', head')
import AlphaConversion (alpha)
import BetaReduction (beta)

solve :: TermP -> Maybe TermS
solve = beta . alpha . toTermS

main :: IO ()
main = do
  -- print "sem1"
  s <- read <$> getLine
  print $ solve s
