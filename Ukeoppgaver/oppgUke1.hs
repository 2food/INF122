-- A se filer "oppgKap1.hs" og "oppgKap2.hs"

-- B

-- 1
plu :: [Int] -> Int -> [Int]
plu [] n = []
plu (k:ks) n = k+n : plu ks n

-- 2
pali :: String -> Bool
pali [] = True
pali [c] = True
pali (c:cs) = if c /= last cs 
    then False
    else pali (init cs)

-- 3
import Test.quickCheck
-- funsksjonene plu og pali er ikke logisk å sjekke med quickCheck


-- C
revel :: [[a]] -> [[a]]
revel [] = []
revel (x:xs) = reverse x : revel xs

-- D

-- 1
del :: Int -> [Int]
del n = [ i | i <- [1..n] , mod i 3 == 0 || mod i 5 == 0]

-- 2
dell :: Int -> [Int] -> [Int]
dell n (x:xs) = [ i | i <- [1..n] , recmod i (x:xs)]
    where
        recmod i [] = False
        recmod i (y:ys) = mod i y == 0 || recmod i ys