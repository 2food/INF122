import Data.Char
import Data.List
import System.IO
import System.Random

size :: Int
size = 3

data Player = O | B | X deriving (Eq, Ord, Show)

type Grid = [[Player]]

nextPlayer :: Player -> Player
nextPlayer O = X
nextPlayer B = B
nextPlayer X = O

empty :: Grid
empty = replicate size (replicate size B)

full :: Grid -> Bool
full = all (/=B) . concat

turn :: Grid -> Player
turn g = if os <= xs then O else X
    where
        os = length (filter (==O) ps)
        xs = length (filter (==X) ps)
        ps = concat g

wins :: Player -> Grid -> Bool
wins p g = any line (rows ++ cols ++ dias)
    where
        line = all (== p)
        rows = g
        cols = transpose g
        dias = [diag g, diag (map reverse g)]

diag :: Grid -> [Player]
diag g = [ g !! n !! n | n <- [0..size-1]]

won :: Grid -> Bool
won g = wins O g || wins X g

putGrid :: Grid -> IO ()
putGrid = putStrLn . unlines . concat . interleave bar . map showRow
    where bar = [replicate ((size*4)-1) '-']

showRow :: [Player] -> [String]
showRow = beside . interleave bar . map showPlayer
    where
        beside = foldr1 (zipWith (++))
        bar    = replicate 1 "|"

showPlayer :: Player -> [String]
showPlayer O = [" O "]
showPlayer B = ["   "]
showPlayer X = [" X "]

interleave :: a -> [a] -> [a]
interleave c []     = []
interleave x [y]    = [y]
interleave x (y:ys) = y : x : interleave x ys

valid :: Grid -> Int -> Bool
valid g i = 0 <= i && i < size^2 && concat g !! i == B

move :: Grid -> Int -> Player -> [Grid]
move g i p = if valid g i
    then [chop size (xs ++ [p] ++ ys)] else []
    where (xs,B:ys) = splitAt i (concat g)

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

getNat :: String -> IO Int
getNat prompt = do
    putStr prompt
    xs <- getLine
    if xs /= [] && all isDigit xs then
        return (read xs)
    else do
        putStrLn "ERROR: Invalid number"
        getNat prompt

tictactoe :: IO ()
tictactoe = run empty O

run :: Grid -> Player -> IO ()
run g p = do
    cls
    goto (1,1)
    putGrid g
    run' g p

goto :: (Int,Int) -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

cls :: IO ()
cls = putStr "\ESC[2J"

run' :: Grid -> Player -> IO ()
run' g p | wins O g  = putStrLn "Player O wins!\n"
         | wins X g  = putStrLn "Player X wins!\n"
         | full g    = putStrLn "It's a draw!\n"
         | otherwise = do
             i <- getNat (prompt p)
             case move g i p of
                 [] -> do putStrLn "ERROR: Invalid move"
                          run' g p
                 [g'] -> run g' (nextPlayer p)

prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter you move: "

data Tree a = Node a [Tree a]   deriving Show

gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [gametree g' (nextPlayer p) | g' <- moves g p]

moves :: Grid -> Player -> [Grid]
moves g p
    | won g = []
    | full g = []
    | otherwise = concat [move g i p | i <- [0..((size^2)-1)]]

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _)  = Node x []
prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]

depth :: Int
depth = 9

minimax :: Player -> Tree Grid -> Tree (Grid,Player)
minimax _ (Node g [])
    | wins O g  = Node (g,O) []
    | wins X g  = Node (g,X) []
    | otherwise = Node (g,B) []
minimax c (Node g ts)
    | c == O = Node (g, minimum ps) ts'
    | c == X = Node (g, maximum ps) ts'
    where
        ts' = map (minimax (nextPlayer c)) ts
        ps = [p | Node (_,p)_ <- ts']

bestmove :: Tree (Grid,Player) -> Grid -> Player -> Grid
bestmove (Node (_,best) ts) g p =
    quickest [ Node (g',p') ts' | Node (g',p') ts' <- ts, p' == best]

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStr "Do you want to go first? (y/n): "
    c <- getChar
    putStrLn "\nGenerating game tree..."
    case c of
        'y'       -> play empty (minimax O $! (gametree empty O)) O
        'n'       -> play empty (minimax X $! (gametree empty X)) X
        otherwise -> do putStrLn "ERROR: invalid input"
                        main

play :: Grid -> Tree (Grid, Player) -> Player -> IO ()
play g tree p = do
    cls
    goto (1,1)
    putGrid g
    play' g tree p

play' :: Grid -> Tree (Grid, Player) -> Player -> IO ()
play' g tree p
    | wins O g = putStrLn "Player O wins!\n"
    | wins X g = putStrLn "Player X wins!\n"
    | full g   = putStrLn "It's a draw!\n"
    | p == O   = do
        i <- getNat (prompt p)
        case move g i p of
            [] -> do putStrLn "ERROR: Invalid move"
                     play' g tree p
            [g'] -> play g' (nextTree tree g') (nextPlayer p)
    | p == X   = do putStr "Player X is thinking ..."
                    let bm = bestmove tree g p
                    (play $! bm) (nextTree tree bm) (nextPlayer p)

-- 1

countNodes :: Tree a -> Int
countNodes (Node _ []) = 1
countNodes (Node _ ts) = 1 + sum (map countNodes ts)

howDeep :: Tree a -> Int
howDeep (Node _ []) = 0
howDeep (Node _ ts) = 1 + maximum (map howDeep ts)

-- 3

quickest :: [Tree (Grid,Player)] -> Grid
quickest [(Node (g,_) _)]      = g
quickest (t1:t2:ts) =
    if howDeep t1 <= howDeep t2 then
        quickest (t1:ts)
    else
        quickest (t2:ts)

-- 4

nextTree :: Tree (Grid, Player) -> Grid -> Tree (Grid, Player)
nextTree (Node (g,p) []) _    = (Node (g,p) [])
nextTree (Node (g,p) ts) move =
    head [ Node (g',p') ts' | Node (g',p') ts' <- ts, g' == move]
