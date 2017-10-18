import Data.List

-- 1

data Nat = Zero | Succ Nat deriving (Show, Read)

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

mult :: Nat -> Nat -> Nat
mult Zero _ = Zero
mult (Succ x) y = add y (mult x y)

-- 2

-- data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving (Eq, Ord, Show, Read)

-- occurs :: Ord a => a -> Tree a -> Bool
-- occurs x (Leaf y)     = x == y
-- occurs x (Node l y r) = case compare x y of
--                         LT -> occurs x l 
--                         EQ -> True
--                         GT -> occurs xl
-- more effective because it only requires one comparison for each node

-- 3

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Eq, Ord, Show, Read)

leafs :: Tree a -> Int
leafs (Leaf _) = 1
leafs (Node l r) = leafs l + leafs r

balanced :: Tree a -> Bool
balanced (Leaf _) = True
balanced (Node l r) = case leafs l - leafs r of
                        -1 -> balanced l && balanced r
                        0 -> balanced l && balanced r
                        1 -> balanced l && balanced r
                        _ -> False

-- 4

split :: [a] -> ([a],[a])
split l = ([x | x <- take (length (l) `div` 2) l], [y | y <- drop (length (l) `div` 2) l])

balance :: [a] -> Tree a
balance [x] = Leaf x
balance [x, y] = Node (Leaf x) (Leaf y)
balance xs = let (left, right) = split xs in
            Node (balance left) (balance right)

-- 5

data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val n) = f n
folde f g (Add x y) = g (folde f g x) (folde f g y)

-- 6

eval :: Expr -> Int
eval e = folde id (+) e

size :: Expr -> Int
size e = folde (\x -> 1) (+) e

-- 7


-- instance Eq a => Eq (Maybe a) where
--     (Maybe x) == (Maybe y) = x ==  y

-- instance Eq a => Eq [a] where
--     []     == []     = True
--     []     == _      = False
--     _      == []     = False    
--     (x:xs) == (y:ys) = x == y && xs == ys