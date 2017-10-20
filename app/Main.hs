module Main where

newtype Symbol = Symbol { unSymbol :: String } deriving (Eq,Show,Read)

data TermS = SymS Symbol
           | LamS Symbol TermS
           | AppS TermS TermS
           deriving (Eq,Show,Read)

data TermI = SymI Int
           | LamI TermI
           | AppI TermI TermI
           deriving (Eq,Show,Read)

data TermP = TermP TermS
            | Boolean Bool
            | Iff Bool TermP TermP
            | Not TermP
            | And TermP TermP
            | Or TermP TermP
            --
            | Pair TermP TermP
            | Fst TermP
            | Snd TermP
            deriving (Eq,Show,Read)

sym x = SymS (Symbol x)
lam x t = LamS (Symbol x) t
app t1 t2 = AppS t1 t2

-- (lam "x" $ app (lam "x" $ sym "x") (lam "x" $ lam "y" $ app (sym "y") (lam "y" $ sym "x")))
toTermI :: TermS -> TermI
toTermI term = toTermI' [] term
  where
    toTermI' args (SymS x) = SymI (getIndex x args)
    toTermI' args (LamS x term) = LamI (toTermI' ([x] ++ args) term)
    toTermI' args (AppS term1 term2) = AppI (toTermI' args term1) (toTermI' args term2)

getIndex :: (Eq a) => a -> [a] -> Int
getIndex a xs = getIndex' 0 xs
  where
    getIndex' i [] = i
    getIndex' i (x:xs) | a == x    = i
                       | otherwise = getIndex' (i + 1) xs

betaI :: TermI -> Maybe TermI
betaI term = error "implement me"
-- betaI term = betaI' term
  -- where
  -- betaI' (SymI x) = x
  -- betaI' (LamI term) = LamI (betaI' term)
  -- betaI' (AppI term1 term2) = betaI'' term1 term2

-- betaI'' :: TermI -> TermI -> Maybe TermI
-- betaI'' ((SymI x) (TermI term)) =
-- betaI'' ((LamI term) (TermI term)) =

--TermS example: (lam "x" $ app (lam "x" $ sym "x") (lam "x" $ lam "y" $ app (sym "y") (lam "y" $ sym "x")))
toTermS :: TermP -> TermS
-- toTermS term = "toTermS"
toTermS (Fst term) = app (toTermS term) (lam "t" $ lam "f" $ sym "t")
toTermS (Snd term) = app (toTermS term) (lam "t" $ lam "f" $ sym "f")
-- toTermS (Pair term)
--
toTermS (Boolean b) = if b then (lam "t" $ lam "f" $ sym "t") else (lam "t" $ lam "f" $ sym "f")
-- toTermS (Iff b term1 term2) = lam
-- toTermS (Not term) =
-- toTermS (And term1 term2) =
-- toTermS (Or term1 term2)
--
toTermS (TermP term) = term


solve :: TermP -> Maybe TermI
solve term = betaI $ toTermI $ toTermS term


main :: IO ()
main = do
  print "sem1"
  -- s <- readLnStr
  -- print $ solve s
