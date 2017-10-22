module Main where

-- x = 2*2

-- y = x

f x = x*x

g x y = x*x + y + f 2

fac n = if n == 0 then 1 else n * fac (n - 1)

iif :: Bool -> t -> t -> t
iif b x y = if b then x else y

iifT = iif True

h0 n = fac n * fac n

h1 n = let
  f = fac n
  in f * f

a = if 0 /= 1 then 2 else 3

b = if not (0 == 1) then 2 else 3

-- 12 `fun` 10 - инфиксный оператор
fun x y = 12*x + 34*y

main :: IO ()
main = putStrLn "hello world"


fibanachi n = fib n (0, 1)
fib 0 (a, _) = a
fib n (a, b) = if n == 0 then a else fib (n-1) (b, a + b)

-- вводим свой тип
data Tri= ON 
        | OFF 
        | UNKNOWN
        deriving (Eq, Read, Show, Ord)

data USD = USD Int deriving (Eq, Read, Show, Ord)

data Point = Point Double Double deriving (Eq, Read, Show, Ord)

data Foo = Foo {getInt :: Int }
         | Bar {getBarInt :: Int }
         | Baz {getPoint :: Point }
         | Quux {getUsd :: USD }
         deriving (Eq, Read, Show, Ord)

foo :: Foo -> Double
foo (Foo f) = fromIntegral f
foo (Bar b) = fromIntegral $ b * 2
foo (Baz (Point 0 y)) = y * y * y
foo (Baz (Point x y)) = x * x + y * y
foo (Quux usd) = fromIntegral $ toRub usd
foo _ = 1

toString :: Tri -> String
toString ON = "On"
toString OFF = "Off"
toString UNKNOWN = "Unknown"

dist :: Point -> Double
dist (Point x y) = sqrt $ x*x + y*y

toRub :: USD -> Int
toRub (USD x) = x * 60 




data Pair a = Pair { first :: a, second :: a }
-- (a, b)
-- data Maybe a = Nothing | Just
-- data Either a b = Left a | Right b

data DayOfWeek = Monday
               | Tuesday
               | Wednesday
               | Thirsday
               | Friday
               | Saturday
               | Sunday
               deriving (Eq, Read, Show, Ord)

data Date = Date { year :: Integer, month :: Int, day :: Int }
isDateValid :: Date -> Bool
isDateValid (Date year month day) = if day > 31 then False else 
									if month == 2 && day > 28 then False else
									if month == 4 || month == 6 || month == 9 || month == 11 then False else True


setDayIn2017_1_x :: Int -> Date
setDayIn2017_1_x day = Date 2017 1 day

setMonthIn2017_x_2 :: Int -> Date
setMonthIn2017_x_2 = \month -> Date 2017 month 2

setMonthYear = \month year -> Date year month 1

twice :: (Int -> Int) -> (Int -> Int)
twice f x = f (f x)

-- data Tree a = Leaf { val :: a }
			-- | Node {...}


-- data [a] = [] | (:) a [a]
list :: [Int]
list = [1, 2, 3, 4, 5]

len :: [a] -> Int
len [] = 0
len (x:xs) = 1 + len xs


maxDate :: [Date] -> Maybe Date
maxDate [] = Nothing
maxDate [d] = Just d
maxDate (d : ds) = Just $ let 
			lt (Date y1 m1 d1) (Date y2 m2 d2) = y1 < y2 ||
												 y1 == y2 && m1 < m2 ||
												 y1 == y2 && m1 == m2 && d1 < d2

			Just maxDs = maxDate ds
			in if d `lt` maxDs
				then maxDs
				else d


------------------------------------------------------------
-- Классы типов (typeclasses)

class Currency a where 
	toRubles :: a -> Int

instance Currency USD where
	toRubles (USD n) = 60 * n

data Euro = Euro Int
instance Currency Euro where
	toRubles (Euro n) = 70 * n

instance Show Date where
	show (Date y m d) = show y ++ "-" ++ show m ++ "-" ++ show d

instance Eq Date where
	(==) d1 d2 = year d1 == year d2 &&
				 month d1 == month d2 &&
				 day d1 == day d2

instance Ord Date where
	compare d1 d2 = let
		yc = compare (year d1) (year d2)
		mc = compare (month d1) (month d2)
		dc = compare (day d1) (day d2)
		in if yc /= EQ
			then yc
			else if mc /= EQ
				then mc
				else dc


instance Show a => Show(Pair a) where
	show (Pair a b) = "[" ++ show a ++ "," ++ show b ++ "]"


----------------------------------------------------------------

list2 :: [Int]
list2 = [6,7,8,9,10]


map2 f = foldl (\ds _c -> ds) []


bmi :: Float -> Float -> String  
bmi weight height 
	| r <= 18.5 = "Underweight"
    | r <= 25.0 = "Normal"
    | r <= 30.0 = "Overweight"
    | otherwise = "Obese"
    where r = weight / (height/100) ^ 2




data Razor
  = Lit Int
  | Add Razor Razor
  
interpret :: Razor -> Int
interpret (Lit n) = n
interpret (Add n1 n2) = interpret n1 + interpret n2

pretty :: Razor -> String
pretty (Lit n) = show n
pretty (Add n1 n2) = "(" ++ pretty n1 ++ " + " ++ pretty n2 ++ ")"



















































