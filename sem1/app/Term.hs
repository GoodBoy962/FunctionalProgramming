module Term (
Symbol (..), TermS (..), TermP (..), sym, lam, app, toTermS, tru, fls, iff', not', or', and', cons', head') where

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

sym x = SymS (Symbol x)
lam x t = LamS (Symbol x) t
app t1 t2 = AppS t1 t2

data TermP = TermP TermS
            -- (3)
            | Boolean Bool
            | Iff TermP TermP TermP
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

tru = lam "t" $ lam "f" $ sym "t"
fls = lam "t" $ lam "f" $ sym "f"
iff' b x y = app (app (app (lam "b" $ lam "x" $ lam "y" $ app (app (sym "b") (sym "x")) (sym "y")) (b)) (x)) (y)
not' x = lam "x" $ app (app (x) (fls)) (tru)
or' x y = lam "x" $ lam "y" $ app (app (x) (tru)) (y)
and' x y = lam "x" $ lam "y" $ app (app (x) (y)) (fls)

cons' = lam "h" $ lam "t" $ lam "c" $ lam "n" $ app (app (sym "c") (sym "h")) (app (app (sym "t") (sym "c")) (sym "n"))
-- nil' t = app (lam "c" $ lam "n" $ sym "n") (toTermS t)
-- isNil' t =
head' = lam "l" $ app (app (sym "l") (lam "h" $ lam "t" $ sym "h")) (fls)
-- tail' =
termP p = TermP p

toTermS :: TermP -> TermS
-- toTermS (Cons term1 term2) =
-- toTermS (IsNil term) = isNil' term
-- toTermS (Head term) =
-- toTermS (Tail term) =
toTermS (Boolean b) = if b then tru else fls
toTermS (Iff b term1 term2) = iff' (toTermS b) (toTermS term1) (toTermS term2)
toTermS (Not term) = not' (toTermS term)
toTermS (And term1 term2) = and' (toTermS term1) (toTermS term2)
toTermS (Or term1 term2) = or' (toTermS term1) (toTermS term2)
toTermS (TermP term) = term
