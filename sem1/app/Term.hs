module Term (Symbol (..), TermS (..), TermP (..), sym, lam, app) where

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
            | Iff Bool TermP TermP
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
