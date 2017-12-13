module Solution where

import Data.Either
import Types

type Env = [(Symbol, Type)]

typeOf :: Term -> Either String Type
typeOf = typeOf' []
  where
    typeOf' env t = case t of

      -- Basic Lambda expressions

      Sym x -> lookup' x env

      Lam x t1Type t -> case typeOf' (extend (x, t1Type) env) t of
        Right t2Type -> Right $ Fun t1Type t2Type
        _            -> Left "type mismatch in lambda"

      App t1 t2 -> case t1Type of
        Right (Fun t1Type' t2Type') | Right t1Type' == t2Type -> Right t2Type'
                                    | otherwise               -> Left "type mismatch in arguments of operator App"
        _ -> Left "type mismatch in first term of application"
        where
          t1Type = typeOf' env t1
          t2Type = typeOf' env t2
      --

      -- Logical
      Boolean _ -> Right Bool

      Not t | typeOf' env t /= Right Bool -> Left "argument of Not is not a boolean"
            | otherwise                   -> Right Bool

      And t1 t2 | t1Type /= Right Bool -> Left "first argument of And is not a boolean"
                | t2Type /= Right Bool -> Left "second argument of And is not a boolean"
                | otherwise            -> Right Bool
                  where
                    t1Type = typeOf' env t1
                    t2Type = typeOf' env t2

      Or t1 t2 | t1Type /= Right Bool -> Left "first argument of Or is not a boolean"
               | t2Type /= Right Bool -> Left "second argument of Or is not a boolean"
               | otherwise            -> Right Bool
                  where
                    t1Type = typeOf' env t1
                    t2Type = typeOf' env t2

      Iff t1 t2 t3 | t1Type /= Right Bool   -> Left "condition guard is not a boolean"
                   | t2Type /= t3Type       -> Left "condition arms are with different types"
                   | otherwise              -> t2Type
                      where
                        t1Type = typeOf' env t1
                        t2Type = typeOf' env t2
                        t3Type = typeOf' env t3
      --

      -- Natural numbers
      Natural n -> if n >= 0 then Right Nat else Left "not natural number"

      Add t1 t2 | t1Type /= Right Nat -> Left "first argument of Add is not a nutural number"
                | t2Type /= Right Nat -> Left "second argument of Add is not a nutural number"
                | otherwise           -> Right Nat
                where
                  t1Type = typeOf' env t1
                  t2Type = typeOf' env t2

      Mult t1 t2 | t1Type /= Right Nat -> Left "first argument of Mult is not a nutural number"
                 | t2Type /= Right Nat -> Left "second argument of Mult is not a nutural number"
                 | otherwise           -> Right Nat
                 where
                   t1Type = typeOf' env t1
                   t2Type = typeOf' env t2
      --

      -- Pairs
      Pair t1 t2 -> case typeOf' env t1 of
        Left _    -> Left "pair first element type mismatch"
        Right ty1 -> case typeOf' env t2 of
          Left _    -> Left "pair second element type mismatch"
          Right ty2 -> Right $ PairT ty1 ty2

      Fst t | isLeft type' -> Left "Fst argument type mismatch"
            | otherwise -> type'
            where
              type' = typeOf' env t

      Snd t | isLeft type' -> Left "Snd argument type mismatch"
            | otherwise -> type'
            where
              type' = typeOf' env t
      --

      -- List
      Nil t -> Right $ List t

      IsNil t -> case typeOf' env t of
        Right (List _) -> Right Bool
        _              -> Left "argument of IsNil is not a list"

      Head t -> case typeOf' env t of
        Right (List t') -> Right t'
        _               -> Left "argument of Head is not a list"

      Tail t -> case typeOf' env t of
        Right (List t') -> Right $ List t'
        _               -> Left "argument of Tail is not a list"

      Cons t1 t2  -> case typeOf' env t2 of
        Right (List t2Type) -> case typeOf' env t1 of
          (Right t1Type) | t1Type /= t2Type -> Left "first argument of Cons must have same type as the elements of the second argument"
                         | otherwise        -> Right $ List t1Type
          _ -> Left "first argument of Cons type mismatch"
        _ -> Left "second argument of Cons is not a list"

--

lookup' :: Symbol -> Env -> Either String Type
lookup' x env = case lookup x env of
  Just t  -> Right t
  Nothing -> Left $ "variable " ++ show x ++ " not in scope"

extend :: (Symbol, Type) -> Env -> Env
extend x env = x : env

-- Examples
--
-- > typeOf $ Lam "x" Nat $ Add (Sym "x") (Natural 5)
-- Right (Fun Nat Nat)

-- > typeOf $ Lam "x" Bool $ Sym "x"
-- Right (Fun Bool Bool)

-- > typeOf $ Add (Natural 5) (Boolean False)
-- Left "..."

-- > typeOf $ App (Lam "x" Nat $ Sym "x") (Natural 5)
-- Right Nat

-- > typeOf $ App (Lam "x" Nat $ Boolean False) (Natural 5)
-- Right Bool

-- > typeOf $ App (Lam "x" Bool $ Boolean False) (Natural 5)
-- Left "..."

-- > typeOf $ Nil Nat
-- Right (List Nat)

-- > typeOf $ Cons (Natural 5) $ Cons (Boolean False) $ Nil Nat
-- Left "..."
