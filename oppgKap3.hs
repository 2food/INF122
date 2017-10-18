-- 1

abclist :: [Char]
abclist = ['a','b','c'] 

abctuple :: (Char,Char,Char)
abctuple = ('a','b','c')

boolchartuplelist  :: [(Bool,Char)] 
boolchartuplelist = [(False,'0'),(True,'1')]

boollistcharlisttuple :: ([Bool],[Char])
boollistcharlisttuple = ([False,True],['0','1'])

listfuncs :: [[a]->[a]] 
listfuncs = [tail,init,reverse]

-- 2

bools :: [Bool]
bools = [True,False]

nums :: [[Int]]
nums = [[1,2],[3]]

add :: Int -> Int -> Int -> Int
add x y z = x + y + z

copy :: a -> (a,a)
copy x = (x,x)

apply :: (a -> b) -> a -> b
apply func x = func x

-- 3

second :: [a] -> a
second xs = head (tail xs)

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

pair :: a -> b -> (a,b)
pair x y = (x,y)

double :: Num a => a -> a
double x = x^2

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (a->a) -> a -> a
twice f x = f (f x)

-- 4
-- Done

-- 5
{-
The hint says it all. 
Functions can't be equal if they aren't of the same type.
If they are, they are equal if their results are equal.
Thus you can only check if functions are equal 
for one set of input arguments at a time. 
-}