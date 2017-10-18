import Data.List
import Data.Char

-- 1

-- [f x | x <- xs, p x] 

-- map (f).filter (p)

-- 2

--a
myAll :: (a -> Bool) -> [a] -> Bool
myAll p [] = True
myAll p (x:xs) | p x       = myAll p xs
               | otherwise = False

--b
myAny :: (a -> Bool) -> [a] -> Bool
myAny p [] = False
myAny p (x:xs) | p x       = True
               | otherwise = myAny p xs

--c
myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile p [] = []
myTakeWhile p (x:xs) | p x       = x:(myTakeWhile p xs)
                     | otherwise = myTakeWhile p xs

--d
myDropWhile :: (a -> Bool) -> [a] -> [a]
myDropWhile p [] = []
myDropWhile p (x:xs) | p x       = myDropWhile p xs
                     | otherwise = x:(myDropWhile p xs)

-- 3

myMap :: (a -> b) -> [a] -> [b]
myMap f [] = []
myMap f xs = foldr (\y ys -> (f y):ys) [] xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p [] = []
myFilter p (x:xs) = foldr (\y ys -> if p y then y:ys else ys) [] (x:xs)

-- 4

dec2int :: [Int] -> Int
dec2int = foldl (\x y -> 10*x + y) 0

-- 5

mycurry :: ((a, b) -> c) -> (a -> b -> c)
mycurry f = (\a b -> f(a,b))

myuncurry :: (a -> b -> c) -> ((a, b) -> c)
myuncurry f = (\(a,b) -> f a b)

-- 6

unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x | p x       = []
               | otherwise = h x : unfold p h t (t x)

type Bit = Int
chop :: Int -> [Bit] -> [[Bit]]
chop n = unfold (==[]) (take n) (drop n) 

unfold_map :: Eq a => (a -> b) -> [a] -> [b]
unfold_map f = unfold (== []) (f.head) tail

unfold_iterate :: (a -> a) -> a -> [a]
unfold_iterate f = unfold (\x -> False) (\x -> x) f 

-- 7

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = mod n 2 : int2bin (div n 2)

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2*y) 0

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

make_par :: [Bit] -> [Bit] -- input is always list of 8
make_par xs | mod (sum xs) 2 == 1 = 1:xs
            | otherwise           = 0:xs

remove_par :: [[Bit]] -> [[Bit]] -- input is always list of lists of 9
remove_par [] = []
remove_par (x:xs) | head x == 1 = if mod (sum (drop 1 x)) 2 == 1 
                                    then (drop 1 x):(remove_par xs) 
                                    else error "Wrong parity"
                  | head x == 0 = if mod (sum (drop 1 x)) 2 == 0 
                                    then (drop 1 x):(remove_par xs) 
                                    else error "Wrong parity"

encode :: String -> [Bit]
encode = concat.map (make_par.make8.int2bin.ord)

decode :: [Bit] -> String
decode = map (chr.bin2int).remove_par.(chop 9)

transmit :: String -> String
transmit = decode.channel.encode
    where channel = id

-- 8

faulty_channel = tail

faulty_transmit = decode.faulty_channel.encode

-- 9 

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f _ [x] = (f x):[]
altMap f g (x:y:xs) =  (f x):(g y):(altMap f g xs)

-- 10 

luhnDouble x | x*2 > 9   = x*2 -9
             | otherwise = x*2

luhn :: [Int] -> Bool
luhn xs = mod (sum (altMap id luhnDouble (reverse xs))) 10 == 0
