-- 1

-- Inner first and outer first. Possibly inner then outer?

-- 2

{- 
sum [n] = n 
sum [n+1] = n+1 
QED -}

-- 3

myProduct :: Num a => [a] -> a
myProduct [] = 1
myProduct (n:ns) = n * product ns

-- 4

--lower to higheer
qsortlth :: Ord a => [a] -> [a]
qsortlth [] = []
qsortlth (n:ns) = qsortlth smaller ++ [n] ++ qsortlth larger
    where
        smaller = [ m | m<-ns , m<=n]
        larger = [ m | m<-ns , m>n]

--higher to lower
qsorthtl :: Ord a => [a] -> [a]
qsorthtl [] = []
qsorthtl (n:ns) = qsorthtl larger ++ [n] ++ qsorthtl smaller
    where
        smaller = [ m | m<-ns , m<=n]
        larger = [ m | m<-ns , m>n]

-- 5

-- the numbers equal to the first number woul dbe dropped by the list 