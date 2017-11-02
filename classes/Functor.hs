newtype MyMaybe a = MyMaybe (Maybe a)
instance Functor MyMaybe where
  fmap f (MyMaybe (Just a)) = MyMaybe $ Just $ f a
  fmap _ (MyMaybe Nothing) = MyMaybe Nothing

newtype MyFun a b = MyFun (a -> b)
instance Functor (MyFun r) where
  fmap f (MyFun g) = MyFun $ f . g

newtype Parser a = Parser { parse :: String -> [(String, a)] }
instance Functor Parser where
  fmap f (Parser g) = Parser $ fmap (fmap (fmap f)) g
