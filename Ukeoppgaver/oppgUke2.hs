-- 0 se egen fil

-- 1
{-
False :: Bool
5 + 8 :: Num t => t
(+) 2 :: Num t => t -> t
(+2) :: Num t => t -> t
(2+) :: Num t => t -> t
(["foo", "bar"], 'a') :: ([String],Char)
[(True, []), (False, [['a']])] :: [(Bool,[Char])]
\x y -> y !! x :: Int -> [a] -> a
[ take, drop, \x y -> [ y !! x ] ] :: [Int -> [a] -> [a]]
-}

-- 2
{-
e1 :: [Bool]
e1 = [False, True, False]
e2 :: Num t => [[t]]
e2 = [[1,2],[3,4]]
e3 :: Num t => [(String,t)]
e3 = [ (“a”,7) ]
e4 :: Num t => [(Char,t)]
e4 = [ (‘a’,7) ]
e5 :: Num t => t -> t
e5 x = x * 2
e6 :: (a,b) -> a
e6 (x,y) = x
e7 :: a -> (a,a)
e7 x = (x,x) 
-}

-- 3
{-
head :: [a] -> a
length :: [a] -> Int
-}

-- 4
{-
foo1 :: a -> b -> (a,b)
foo1 a b = (a, b)
foo2 :: a -> b -> (a,b)
foo2 a = \b -> (a, b)
foo3 :: a -> b -> (a,b)
foo3 = \a b -> (a, b)
foo4 :: a -> b -> (a,b)
foo4 = \a -> \b -> (a, b)
-}

-- 5
f :: Int -> Int -> Int
f x y = x+y
g :: (Int, Int) -> Int
g (x,y) = x+y