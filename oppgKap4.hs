
-- 1

halve :: [a] -> ([a],[a])
halve xs = (take n xs, drop n xs)
            where n = div (length xs) 2

-- 2

thirda :: [a] -> a
thirda xs = head ( tail ( tail xs ))

thirdb :: [a] -> a
thirdb xs = xs !! 2

thirdc :: [a] -> a
thirdc (_:_:x:_) = x

-- 3

safetaila :: [a] -> [a]
safetaila xs = if null xs 
               then []
               else tail xs

safetailb :: [a] -> [a]
safetailb xs | null xs   = []
             | otherwise = tail xs

safetailc :: [a] -> [a]
safetailc [] = []
safetailc (x:xs) = xs

-- 4
{-
(||) :: Bool -> Bool -> Bool

True || True = True
True || False = True
False || True = True
False || False = False

True || _ = True
_ || True = True
_ || _ = False

True || _ = True
False || b = b

False || False = False
_ || _ = True

-} 

-- 5
{-
a && b = if a == True then 
         if a == b then True else False
         else False
-}

-- 6
{-
a && b = if a == True then b else False
-}

-- 7

mult = \x -> (\y -> (\z -> x*y*z))

-- 8

luhnDouble :: Int -> Int
luhnDouble n | n*2 > 9   = n*2-9
             | otherwise = n*2 

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = if mod (luhnDouble a + luhnDouble b + luhnDouble c + luhnDouble d) 10 == 0 
               then True
               else False
