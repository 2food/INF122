import Data.List
import Data.Char

-- 1
sumsqr :: [Int]
sumsqr = [x^2 | x <- [1..100]]

-- 2
grid :: Int -> Int -> [(Int,Int)]
grid n m = [(x,y) | x <- [0..n], y <- [0..m]]

-- 3
square :: Int -> [(Int,Int)]
square n = [(x,y) | (x,y) <- (grid n n) , x /= y]

-- 4
myreplicate :: Int -> a -> [a]
myreplicate n t = [t | x <- [1..n]]

-- 5
pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

-- 6

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], x == sum (factors x)]
    where
        factors n = [x | x <- [1..(n-1)], mod n x == 0]

-- 7 

origen = [(x,y) | x <- [1,2], y <- [3,4]]

gen = concat [[(x,y) | y <- [3,4]] | x <- [1,2]]

-- 8
myfind :: Eq a => a -> [(a,b)] -> [b]
myfind k t = [v | (k',v) <- t, k == k']

positions x as = myfind x (zip as [1..]) 

-- 9
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x*y | (x,y) <- zip xs ys]

-- 10

llet2int :: Char -> Int
llet2int c = ord c - ord 'a'
ulet2int :: Char -> Int
ulet2int c = ord c - ord 'A'

int2llet :: Int -> Char
int2llet n = chr(ord 'a' + n)
int2ulet :: Int -> Char
int2ulet n = chr(ord 'A' + n)


shift :: Int -> Char -> Char
shift n c | isLower c = int2llet (mod (llet2int c + n) 26)
          | isUpper c = int2ulet (mod (ulet2int c + n) 26)
          | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

