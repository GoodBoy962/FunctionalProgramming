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
            | Pair TermP TermP
            | Fst TermP
            | Snd TermP
            deriving (Eq,Show,Read)

-- let
sym x = SymS (Symbol x)
lam x t = LamS (Symbol x) t
app t1 t2 = AppS t1 t2

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
betaI = error "Implement me"

toTermS :: TermP -> TermS
toTermS = error "Implement me!"

solve :: TermP -> Maybe TermI
solve = error "Choose your variant"



main :: IO ()
main = do
  print "sem1"
--   -- s <- readLnStr
--   -- print $ solve s
