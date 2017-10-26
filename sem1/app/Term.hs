module Term (
Symbol (..), TermS (..), TermI (..), TermP (..),
sym, lam, app,
toTermS,
tru, fls, iff', not', or', and',
cons', head', isNil', tail', nil',
termP) where

newtype Symbol = Symbol { unSymbol :: String } deriving (Eq,Read,Show)

{-
instance Show Symbol where
  show (Symbol x) = show x
-}

data TermS =
      SymS Symbol
    | LamS Symbol TermS
    | AppS TermS TermS
    deriving (Eq,Read,Show)

{-
instance Show TermS where
    show (SymS x)       = "sym " ++ show x
    show (LamS x term)  = "(lam " ++ (show x) ++ " $ " ++ (show term) ++ ")"
    show (AppS term1 term2) = "app (" ++ (show term1) ++ ") (" ++ (show term2) ++ ")"
-}

data TermI =
      SymI Int
    | LamI TermI
    | AppI TermI TermI
    deriving (Eq,Show,Read)

sym x = SymS (Symbol x)
lam x t = LamS (Symbol x) t
app t1 t2 = AppS t1 t2

data TermP =
              TermP TermS
            | Boolean Bool
            | Iff TermP TermP TermP
            | Not TermP
            | And TermP TermP
            | Or TermP TermP
            | Natural Int
            | Plus TermP TermP
            | Mult TermP TermP
            | Minus TermP TermP
            | Divide TermP TermP
            | Y TermP
            | Pair TermP TermP
            | Fst TermP
            | Snd TermP
            | Cons TermP TermP
            | Nil
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

pair f s = lam "f" $ lam "s" $ lam "b" $ app (app (sym "b") (sym "f")) (sym "s")
fst' t = lam "p" $ app (sym "p") tru
snd' t = lam "p" $ app (sym "p") fls

nil' = pair tru tru
cons' h t = app (app (lam "h" $ lam "t" $ pair fls (pair h t)) h) t
isNil' t = fst' t
head' z = app (lam "z" $ fst' (snd' (sym "z"))) z
tail' z = app (lam "z" $ snd' (snd' (sym "z"))) z

termP p = TermP p

toTermS :: TermP -> TermS
--
toTermS (Boolean b) = if b then tru else fls
toTermS (Iff b term1 term2) = iff' (toTermS b) (toTermS term1) (toTermS term2)
toTermS (Not term) = not' (toTermS term)
toTermS (And term1 term2) = and' (toTermS term1) (toTermS term2)
toTermS (Or term1 term2) = or' (toTermS term1) (toTermS term2)
--
toTermS (Pair term1 term2) = pair (toTermS term1) (toTermS term2)
toTermS (Fst term) = fst' (toTermS term)
toTermS (Snd term) = snd' (toTermS term)
--
toTermS Nil = nil'
toTermS (Cons term1 term2) = cons' (toTermS term1) (toTermS term2)
toTermS (IsNil term) = isNil' $ toTermS term
toTermS (Head term) = head' $ toTermS term
toTermS (Tail term) = tail' $ toTermS term
--
toTermS (TermP term) = term
