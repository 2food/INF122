main :: IO Int
main = return 0


-- 1
data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

instance Functor Tree where
  fmap _ Leaf         = Leaf
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

-- 2
-- instance Functor ((->) a) where
--   fmap :: (b -> c) -> (a -> b) -> (a -> c)
--   fmap = (.)

-- 3

-- instance Applicative ((->) a) where
--   pure :: b -> (a -> b)
--   pure = const
--   <*> :: (a -> (b -> c)) -> (a -> b) -> (a -> c)
--   g <*> h = \x -> g x (h x)

-- 4
newtype ZipList a = Z [a] deriving Show

instance Functor ZipList where
  -- fmap :: (a-> b) -> ZipList a -> ZipList b
  fmap g (Z xs) = Z [g x | x <- xs]

instance Applicative ZipList where
  -- pure :: a -> ZipList a
  pure x = Z [x]

  -- <*> :: ZipList (a -> b) -> ZipList a -> Ziplist b
  (Z gs) <*> (Z xs) = Z [g x | x <- xs, g <- gs]
