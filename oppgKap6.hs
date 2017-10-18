-- 1
fac :: Int -> Int
fac 0 = 1
fac n | n > 0 = n * fac (n-1)
      | otherwise = error "function fac dous not take negative numbers"

-- 2
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)

-- 3
{-
n (^) 0 = 1
n (^) m = n * n^m

2^3 = 2*2^2 = 2*2*2^1 = 2*2*2*2^0 = 2*2*2*1 = 8
-}

-- 4
euclid :: Int -> Int -> Int
euclid n m | n == m    = n
           | n < m     = euclid n (m-n)
           | otherwise = euclid (n-m) m

-- 5
{-
length [1,2,3] 
= 1+ length [2,3]
= 1+1+ length [3]
= 1+1+1+ length []
= 1+1+1+0 = 3

drop 3 [1,2,3,4,5]
= drop 2 [2,3,4,5]
= drop 1 [3,4,5]
= drop 0 [4,5] = [4,5]

init [1,2,3] 
= 1:(init [2,3])
= 1:(2:(init [3]))
= 1:(2:[]) = [1,2]
-}

-- 6

myand :: [Bool] -> Bool
myand [] = True
myand (False:xs) = False
myand (_:xs) = myand xs

myconcat :: [[a]] -> [a]
myconcat [] = []
myconcat (x:xs) = x ++ myconcat xs

myreplicate :: Int -> a -> [a]
myreplicate 0 x = []
myreplicate n x = x:(myreplicate (n-1) x)

(!!!) :: [a] -> Int -> a
(!!!) (x:xs) 0 = x 
(!!!) [] _ = error "Index too large"
(!!!) (x:xs) n | n >= 0 = (!!!) xs (n-1) 
               | otherwise = error "Negative index"

myelem :: Eq a => a -> [a] -> Bool
myelem _ [] = False
myelem e (x:xs) | e == x = True
                | otherwise = myelem e xs

-- 7

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) | x <= y = x:(merge xs (y:ys))
                    | otherwise = y:(merge (x:xs) ys)

-- 8

halve :: [a] -> ([a],[a])
halve xs = (take n xs, drop n xs)
    where n = div (length xs) 2 

msort :: Ord a => [a] -> [a] 
msort [] = []
msort [x] = [x]
msort xs = merge (msort (fst h)) (msort (snd h))
    where h = halve xs 

-- 9

mysum :: Num n => [n] -> n
mysum [] = 0
mysum (n:ns) = n + sum ns 

mytake :: Integral i => i -> [a] -> [a]
mytake 0 _ = []
mytake n [] = error "Index to large"
mytake n (x:xs) = x:(mytake (n-1) xs)

mylast :: [a] -> a
mylast [] = error "Empty list"
mylast [x] = x
mylast (x:xs) = mylast xs