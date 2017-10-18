module Kap9
    (
    ) where

-- 1 meeeh

-- 2

remove :: Eq a => a -> [a] -> [a]
remove x []     = []
remove x (y:ys) | x == y    = ys
                | otherwise = y : remove x ys

isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] _        = True
isChoice (x:xs) ys | x `elem` ys = isChoice xs (remove x ys)
                   | otherwise   = False


-- 3 more similar solutions
