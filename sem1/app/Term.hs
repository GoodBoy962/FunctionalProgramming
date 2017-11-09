module Term (
Symbol (..), TermS (..), TermI (..), TermP (..),
sym, lam, app,
tru, fls, factorial, natural,
toTermS) where

newtype Symbol = Symbol { unSymbol :: String } deriving (Eq,Read,Show)

data TermS =
      SymS Symbol
    | LamS Symbol TermS
    | AppS TermS TermS
    deriving (Eq,Read,Show)

data TermI =
      SymI Int
    | LamI TermI
    | AppI TermI TermI
    deriving (Eq,Show,Read)

sym x = SymS (Symbol x)
lam x = LamS (Symbol x)
app = AppS

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
-- iff b x y = app (app (app (lam "b" $ lam "x" $ lam "y" $ app (app (sym "b") (sym "x")) (sym "y")) (b)) (x)) (y)
iff = lam "b" $ lam "t" $ lam "f" (app (app (sym "b") (sym "t")) (sym "f"))
not_ = lam "x" $ app (app (sym "x") fls) tru
and_ = lam "x" $ lam "y" (app (app (sym "x") (sym "y")) fls)
or_ = lam "x" $ lam "y" $ app (app (sym "x") tru) (sym "y")

pair = lam "f" $ lam "s" (lam "b" (app (app (sym "b") (sym "f")) (sym "s")))
fst_ = lam "p" $ app (sym "p") tru
snd_ = lam "p" $ app (sym "p") fls

nil = lam "c" $ lam "n" $ sym "n"
cons = lam "h" $ lam "t" $ lam "c" $ lam "n" $ app (app (sym "c") (sym "h")) (app (app (sym "t") (sym "c")) (sym "n"))
isnil = lam "l" $ app (app (sym "l") (lam "h" (lam "t" fls))) tru
head_ = lam "l" (app (app (sym "l") (lam "h" (lam "t" (sym "h")))) fls)
tail_ = lam "l" (app fst_ (app (app (sym "l") (lam "x" (lam "p" (app (app pair (app snd_ (sym "p"))) (app (app cons (sym "x")) (app snd_ (sym "p"))))))) (app (app pair nil) nil)))

omega = lam "x" $ app (sym "f") (lam "y" $ app (app (sym "x") (sym "x")) (sym "y"))
y' = app (lam "f" $ app omega omega)

natural n = lam "s" $ lam "z" $ numberToTermS n
mult t1 t2 = app (app (lam "x" $ lam "y" $ lam "s" $ lam "z" $ app (app (sym "x") (app (sym "y") (sym "s"))) (sym "z")) (toTermS t1)) (toTermS t2)

factorial = lam "fac" $ lam "n" $ iif' (isZero $ sym "n")
    (natural 1)
    (mult (TermP $ sym "n") (TermP $ app (sym "fac") (pred' (sym "n"))))

iif' b x = app (app (app (lam "b" $ lam "x" $ lam "y" $ app (app (sym "b") (sym "x")) (sym "y")) b) x)

pred' = app (lam "n" $ lam "s" $ lam "z" $ app (app (app (sym "n") (lam "g" $ lam "h" $ app (sym "h") (app (sym "g") (sym "s")))) (lam "u" $ sym "z")) (lam "u" $ sym "u"))
isZero = app (lam "n" $ app
    (app (sym "n") (lam "x" $ lam "t" $ lam "f" $ sym "f"))
    (lam "t" $ lam "f" $ sym "t"))

numberToTermS :: Int -> TermS
numberToTermS n
    | n == 0 = sym "z"
    | otherwise = app (sym "s") (numberToTermS (n-1))

toTermS :: TermP -> TermS
--
toTermS (TermP t) = t
--
toTermS (Boolean True) = tru
toTermS (Boolean False) = fls
toTermS (Iff b x y) = app (app (app iff (toTermS b)) (toTermS x)) (toTermS y)
toTermS (Not x) = app not_ (toTermS x)
toTermS (And x y) = app (app and_ (toTermS x)) (toTermS y)
toTermS (Or x y) = app (app or_ (toTermS x)) (toTermS y)
--
toTermS (Pair x y) = app (app pair (toTermS x)) (toTermS y)
toTermS (Fst p) = app fst_ (toTermS p)
toTermS (Snd p) = app snd_ (toTermS p)
--
toTermS Nil = nil
toTermS (Cons l t) = app (app cons (toTermS l)) (toTermS t)
toTermS (IsNil l) = app isnil (toTermS l)
toTermS (Head l) = app head_ (toTermS l)
toTermS (Tail l) = app tail_ (toTermS l)
--
toTermS (Y t) = y' $ toTermS t
