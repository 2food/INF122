main :: IO Int
main = return 0
-- A se oppgKap16.hs

-- B
-- Bevis at for enhver ikke-tom liste [x1,x2,…,xn], holder likheten:
-- foldr f v [x1,x2,…,xn] = f x1 (f x2 (… (f xn v) …))

-- precondition: input: funksjon f, basecase v,  xs slik at length(xs) > 0
-- invariant: foldr f v [x1,x2,…,xn] = f x1 (f x2 (… (f xn v) …))

-- foldr f v [x] = f x v , riktig
-- foldr f v (x:xs) = f x (foldr f v xs)
-- anta: (foldr f v xs) = f x1 (f x2 (… (f xn v) …)) for alle x i xs
-- da er
-- foldr f v (x:xs) = f x (foldr f v xs) = f x (f x1 (f x2 (… (f xn v) …)))
-- og vi har vist at invariant gjelder for alle ikke-tomme lister

-- C

-- 1
bsum :: Fractional t => Int -> t
bsum 1 = 1 / (1 * 2)
bsum n = 1 / fromIntegral(n * (n + 1)) + bsum (n-1)

-- 2 og 3
-- precondition: input n > 0
-- postcondition: sum (1/(x*(x+1))) for all x in [1..n]
-- invariant: bsum n oppfuller postcondition

-- bsum 1 = 1 / (1 * 2) = 0.5  , riktig
-- bsum n = 1 / (n * (n+1)) + bsum (n-1)
-- anta: bsum (n-1) = sum (1/(x*(x+1))) for all x in [1..(n-1)]
-- da er
-- bsum n = 1 / (n * (n+1)) + bsum (n-1) , riktig

-- 3
bsum' :: Fractional t => Int -> t
bsum' n = sum [1 / fromIntegral(x * (x + 1)) | x <- [1..n]]

-- D
sub :: Eq t => [t] -> [t] -> [t]
sub xs ys = [x | x <- xs, x `notElem` ys]

-- Bevis at resulterende liste ikke inneholder noen elementer fra den andre argumentlisten.
-- dah siden (x notElem ys) der et kriterie for at x er med i resultatlisten
-- betyr at alle elementer i resultatlisten ikke er med i ys.

-- E Funksjonen
-- map :: (a -> b) -> [a] -> [b]
-- er definert ved:
-- map f [] = []
-- map f (x:xs) = (f x) : map f xs
-- Bevis, ved induksjon på listens lengde at:
-- 1. map id = id
-- dvs. at for enhver liste xs, holder likheten: map id xs = xs, og
-- 2. map (f . g) = (map f) . (map g)
-- dvs. at for enhver liste xs, har vi: map (f . g) xs = (map f) . (map g) xs.

-- map id = id
-- map id [] = [] , definisjon av map
-- map id (x:xs) = (id x) : map id xs , definisjon av map
--               = (id x) : (id xs) , anta at invariant stemmer for (map id xs)
--               = x:xs , definisjon av id
-- map id (x:xs) = x:xs , bevist

-- map (f . g) = (map f) . (map g)
-- map (f . g) [] = [] , definisjon av map
-- map (f . g) (x:xs) = ((f . g) x) : map (f . g) xs , definisjon av map
--                    = ((f . g) x) : ((map f) . (map g) xs) , anta at invariant stemmer for (map (f . g) xs)
--                    = map f ((g x) : (map g xs) , definisjion of map)
--                    = map f (map g (x:xs)) , definisjion of map
--                    = (map f) . (map g) (x:xs) , definisjion av composition

-- F

data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving Show

instance Functor Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap g (Leaf x)     = Leaf (g x)
  fmap g (Node l x r) = Node (fmap g l) (g x) (fmap g r)
