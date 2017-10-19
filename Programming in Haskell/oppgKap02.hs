-- 1 

-- bleh

-- 2
{-
(n^3)*4
(n*3)+(4*5)
n+(3*(4^5))
-}

-- 3
n = x `div` (length xs)
    where 
        x = 10
        xs = [1,2,3,4,5]

-- 4
myLast = head.reverse

-- 5
myInit1 = reverse.tail.reverse

myInit2 (x:xs) = take (length (x:xs) -1) (x:xs)
