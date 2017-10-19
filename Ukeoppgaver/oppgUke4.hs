import Data.List
import Data.Char
-- A

sub :: Int -> Int -> [a] -> [a]
sub a b xs = [x | (x,z) <- zip xs [0..], a <= z, z < b ]

-- B

fjern :: String -> Char -> String
fjern xs c = [x | x <- xs, x /= c]

uten_fjern :: String -> Char -> String
uten_fjern [] c = []
uten_fjern (x:xs) c | x == c    = uten_fjern xs c
                    | otherwise = x:(uten_fjern xs c)

-- C

tegnpos :: Char -> String -> [Int]
tegnpos c xs = [y | (x,y) <- zip xs [0..], x == c]

-- D done se oppgKap6.hs

-- E

clean :: String -> String
clean [] = []
clean (x:xs) | isAlpha x = (toLower x):(clean xs)
             | otherwise = clean xs

-- F

tokenize :: String -> String -> String -> [String]
tokenize [] imp rem = []
tokenize [x] imp res = [[x]]
tokenize (x:xs) imp rem 
    | elem x rem = tokenize xs imp rem
    | elem x imp = (x:[]):(tokenize xs imp rem)
    | otherwise  = (\a (b:bs) -> ((a:b):bs)) x (tokenize xs imp rem)