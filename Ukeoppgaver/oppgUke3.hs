-- A Done 

-- B 

fLett :: Int -> Int
fLett 1 = 1
fLett x = x^2 + fLett (x-1)

fListe :: Int -> Int
fListe x = sum[n^2 | n <- [1..x]]

f = \x -> sum[n^2 | n <- [1..x]]

-- C Done

-- D




toListLett :: Int -> [Int]
toListLett x | x < 10    = [x]
             | otherwise = (toListLett (div x 10)) ++ [(mod x 10)]

toList :: Int -> [Int]
toList x = reverse[mod (nth x n) 10 | n <- [1..digits x]]
    where 
        nth x n = (div x (10^(n-1))) -- x / 10^n
        digits n = floor(logBase 10 (fromIntegral n))+1 -- number of digits in n

-- E Done