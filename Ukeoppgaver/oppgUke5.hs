
-- A Se oppgKap7.hs

-- B Se oppgKap8.hs

-- C

-- a
data Exp = S | U | Og Exp Exp | El Exp Exp | Ik Exp

folde :: a -> a -> (a -> a -> a) -> (a -> a -> a) -> (a -> a) -> Exp -> a
folde s u o e i S = s
folde s u o e i U = u
folde s u o e i (Og x y) = o (folde s u o e i x) (folde s u o e i y)
folde s u o e i (El x y) = e (folde s u o e i x) (folde s u o e i y)
folde s u o e i (Ik x) = i (folde s u o e i x)

-- b
evb :: Exp -> Bool
evb = folde True False (&&) (||) not

-- c
evi :: Exp -> Int
evi = folde 1 5 (+) (*) (\x -> -x)

-- d
evh :: Exp -> Int
evh = folde 1 1 (pmaxsub) (pmaxsub) (+1) 
    where pmaxsub = (\x y -> 1+(if x > y then x else y))

-- D

type Ordbok k v = [(k,v)]

finn :: Eq a => a -> Ordbok a b -> b
finn s []                     = error "Fant ikke!"
finn s ((k,v):bs) | s == k    = v
                  | otherwise = finn s bs

settInn :: Eq a => a -> b -> Ordbok a b -> Ordbok a b
settInn x y []                     = (x,y):[]
settInn x y ((k,v):bs) | x /= k    = (k,v): settInn x y bs
                       | otherwise = (x,y):(k,v):bs

endre :: Eq a => a -> b -> Ordbok a b -> Ordbok a b 
endre _ _ []                     = []
endre x y ((k,v):bs) | x == k    = (x,y):bs
                     | otherwise = (k,v): endre x y bs

slettF :: Eq a => a -> Ordbok a b -> Ordbok a b
slettF _ []                     = []
slettF x ((k,v):bs) | x == k    = bs
                    | otherwise = (k,v): slettF x bs

slettAlle :: Eq a => a -> Ordbok a b -> Ordbok a b
slettAlle _ []                     = []
slettAlle x ((k,v):bs) | x == k    = slettAlle x bs
                       | otherwise = (k,v): slettAlle x bs

erLike :: (Eq a, Eq b) => Ordbok a b -> Ordbok a b -> Bool
erLike xs ys = xs == ys
