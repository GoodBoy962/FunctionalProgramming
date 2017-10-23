module AlphaConversion (alpha) where

import Data.Unique
import System.IO.Unsafe (unsafePerformIO)
import Term (Symbol (..), TermS (..), TermP (..), sym, lam, app)

-- (1.1)
-- Rename all veriables to make their names different
alpha :: TermS -> TermS
alpha (SymS x) = SymS x
alpha (AppS term1 term2) = AppS (alpha term1) (alpha term2)
alpha (LamS x term) =
    let newSymbol = (getUniqueSymbol x) in
    LamS newSymbol (alpha' x newSymbol (alpha term))

alpha' :: Symbol -> Symbol -> TermS -> TermS
alpha' toReplace replaceWith (SymS x)
    | x == toReplace = (SymS replaceWith)
    | otherwise = (SymS x)
alpha' toReplace replaceWith (LamS x term) =
    LamS x (alpha' toReplace replaceWith term)
alpha' toReplace replaceWith (AppS term1 term2) =
    AppS (alpha' toReplace replaceWith term1) (alpha' toReplace replaceWith term2)

getUniqueSymbol :: Symbol -> Symbol
getUniqueSymbol (Symbol x) = unsafePerformIO (getUniqueSymbol' (Symbol x))

getUniqueSymbol' :: Symbol -> IO Symbol
getUniqueSymbol' (Symbol x) = do
    unique <- newUnique
    -- let newSymbol = (Symbol ("arg" ++ show (hashUnique unique)))
    let newSymbol = (Symbol (x ++ show (hashUnique unique)))
    return newSymbol
