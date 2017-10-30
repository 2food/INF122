import System.IO
import Data.Char

-- Hangmman
-- hangman :: IO ()
-- hangman = do putStrLn "Think of a word:"
--              word <- sgetLine
--              putStrLn "Try to guess it:"
--              play word
--
-- sgetLine = do x <- getCh
--               if x == '\n' then
--                   do putChar x
--                      sgetLine :: IO String
--                      return []
--               else
--                   do putChar '-'
--                      xs <- sgetLine
--                      return (x:xs)
--
-- getCh :: IO Char
-- getCh = do hSetEcho stdin False
--            x <- getChar
--            hSetEcho stdin True
--            return x
--
-- play :: String -> IO ()
-- play word = do putStr "? "
--                guess <- getLine
--                if guess == word then
--                    putStrLn "You got it!!"
--                else do putStrLn (match word guess)
--                        play word
--
-- match :: String -> String -> String
-- match xs ys = [if elem x ys then x else '-' | x <- xs]


-- Nim
-- next :: Int -> Int
-- next 1 = 2
-- next 2 = 1
--
-- type Board = [Int]
--
-- initial :: Board
-- initial = [5,4,3,2,1]
--
-- finished :: Board -> Bool
-- finished = all (==0)
--
-- valid :: Board -> Int -> Int -> Bool
-- valid board row num = board !! (row-1) >= num
--
-- move :: Board -> Int -> Int -> Board
-- move board row num = [update r n | (r,n) <- zip [1..] board ]
--     where update r n = if r == row then n-num else n
--
-- putRow :: Int -> Int -> IO ()
-- putRow row num = do
--     putStr (show row)
--     putStr ": "
--     putStrLn (concat (replicate num "* "))
--
-- putBoard :: Board -> IO ()
-- putBoard [a,b,c,d,e] = do
--     putRow 1 a
--     putRow 2 b
--     putRow 3 c
--     putRow 4 d
--     putRow 5 e
--
-- getDigit :: String -> IO Int
-- getDigit prompt = do
--     putStr prompt
--     x <- getChar
--     newline
--     if isDigit x then
--         return (digitToInt x)
--     else do
--         putStrLn "ERROR: Invalid digit"
--         getDigit prompt
--
-- newline :: IO ()
-- newline = putChar '\n'
-- lineSeperator :: IO ()
-- lineSeperator = putStrLn (replicate 20 '-')
--
-- play :: Board -> Int -> IO ()
-- play board player = do
--     newline
--     putBoard board
--     if finished board then do
--         newline
--         putStrLn ("Player " ++ show (next player) ++ " wins!!")
--     else do
--         newline
--         putStrLn ("Player " ++ show player)
--         row <- getDigit "Enter a row number: "
--         num <- getDigit "Stars to remove: "
--         if valid board row num then do
--             lineSeperator
--             play (move board row num) (next player)
--         else do
--             newline
--             putStrLn "ERROR: Invalid move"
--             lineSeperator
--             play board player
--
-- nim :: IO ()
-- nim = play initial 1


-- Game of Life
cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int,Int)

writeat :: Pos -> String -> IO ()
writeat p xs = do
    goto p
    putStr xs

goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

width :: Int
width = 10
height :: Int
height = 10

type Board = [Pos]

glider :: Board
glider = [(4,2),(2,3),(4,3),(3,4),(4,4)]

showcells :: Board -> IO ()
showcells b = sequence_ [writeat p "0" | p <- b]

isAlive :: Board -> Pos -> Bool
isAlive b p = elem p b

isEmpty :: Board -> Pos -> Bool
isEmpty b p = not (isAlive b p)

neighbs :: Pos -> [Pos]
neighbs (x,y) = map wrap [(x-1,y-1),(x,y-1),
                          (x+1,y-1),(x-1,y),
                          (x+1,y),(x-1,y+1),
                          (x,y+1),(x+1,y+1)]

wrap :: Pos -> Pos
wrap (x,y) = (((x-1) `mod` width) +1,
              ((y-1) `mod` height) +1)

liveneighbs :: Board -> Pos -> Int
liveneighbs b = length . filter (isAlive b) . neighbs

survivors :: Board -> [Pos]
survivors b = [p | p <- b, elem (liveneighbs b p) [2,3]]

births :: Board -> [Pos]
births b = [p | p <- rmdups (concat (map neighbs b)),
                isEmpty b p,
                liveneighbs b p == 3]

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/=x) xs)

nextgen :: Board -> Board
nextgen b = survivors b ++ births b

life :: Board -> IO ()
life b = do
    cls
    showcells b
    wait 5000000
    life (nextgen b)

wait :: Int -> IO ()
wait n = sequence_ [return () | _ <- [1..n]]

-- -- 1
-- putStr' :: String -> IO ()
-- putStr' s = sequence_ [putChar x | x <- s]
--
-- -- 2
-- type Board = [Int]
--
-- putRow :: Int -> Int -> IO ()
-- putRow row num = do
--     putStr (show row)
--     putStr ": "
--     putStrLn (concat (replicate num "* "))
--
-- putBoard :: Board -> IO ()
-- putBoard = putBoard_rec 1
--
-- putBoard_rec r [] = return ()
-- putBoard_rec r (n:ns) = do
--     putRow r n
--     putBoard_rec (r+1) ns
--
-- -- 3
-- putBoard' :: Board -> IO ()
-- putBoard' b = sequence_ [putRow r n | (r, n)  <- zip [1..] b]
--
-- -- 4
--
-- getDigit :: IO Int
-- getDigit = do
--     x <- getChar
--     putStrLn ""
--     if isDigit x then
--         return (digitToInt x)
--     else do
--         putStrLn "ERROR: Invalid digit"
--         getDigit
--
-- getSum :: Int -> IO Int
-- getSum 0 = return 0
-- getSum n = do
--     x <- getDigit
--     rest <- getSum (n-1)
--     return (x + rest)
--
-- adder :: IO ()
-- adder = do
--     putStr "How many numbers? "
--     n <- getDigit
--     total <- getSum n
--     putStrLn ("The total is " ++ show total)
--
-- -- 5
-- adder' :: IO ()
-- adder' = do
--     putStr "How many numbers? "
--     n <- getDigit
--     values <- sequence [getDigit | counter <- [1..n]]
--     putStrLn ("The total is " ++ show (sum values))
--
-- -- 6 meeeh
