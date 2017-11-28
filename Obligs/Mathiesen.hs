-- Tormod Utslottøy Mathiesen
-- Horisontal slider: s 2 3 b 3 3 20 5 11 6 10 6 9 6 8 5 8 4 8 3 8 2 11 2 9

import           Control.Concurrent (threadDelay)
import           Data.Char          (isDigit)
import           Data.List          (nub)

main :: IO ()
main = do
  cls
  goto (0,0)
  c <- getInitialCommand
  resolve emptyBoard c

mainloop :: Board -> IO ()
mainloop (B s pss surv birt) = do
  c <- getCommand s
  resolve (B s pss surv birt) c

-- terminalcodes http://wiki.bash-hackers.org/scripting/terminalcodes
cls :: IO ()
cls = do
  putStr "\ESC[2J"
  goto (0,0)

resetPrompt :: Int -> IO ()
resetPrompt s = do
  goto (0, s+3)
  putStr "\ESC[2K"

resetInfo :: Int -> IO ()
resetInfo s = do
  goto (0, s+4)
  putStr "\ESC[2K"

goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

writeat :: Pos -> String -> IO ()
writeat p xs = do
    goto p
    putStr xs

type Pos = (Int, Int)
type Size = Int
type SRule = (Int, Int)
type BRule = (Int, Int)
data Board = B Size [Pos] SRule BRule deriving (Eq)

instance Show Board where
  show (B si ps su bi) =
    "s " ++ show (fst su) ++ " " ++ show (snd su) ++ " " ++
    "b " ++ show (fst bi) ++ " " ++ show (snd bi) ++ " " ++
    show si ++ concat [" " ++ show x ++ " " ++ show y | (x,y) <- ps]

parse :: String -> Board
parse [] = error "This should never happen! parse was called on empty string"
parse ('s':r) =
  let [sx,sy] = map read (take 2 (words r)) :: [Int] in
  let [bx,by] = map read (take 2 (drop 3 (words r))) :: [Int] in
  let [s]     = map read (take 1 (drop 5 (words r))) :: [Int] in
  let pss     = pair (map read (drop 6 (words r)) :: [Int]) in
  B s pss (sx,sy) (bx,by)
    where pair :: [Int] -> [Pos]
          pair []       = []
          pair (x:y:xs) = (x,y) : pair xs
          pair [e]      = error ("This should never happen! pair was called on: "++show e)
parse ('b':r) =
  let [bx,by] = map read (take 2 (words r)) :: [Int] in
  let [sx,sy] = map read (take 2 (drop 3 (words r))) :: [Int] in
  let [s]     = map read (take 1 (drop 5 (words r))) :: [Int] in
  let pss     = pair (map read (drop 6 (words r)) :: [Int]) in
  B s pss (sx,sy) (bx,by)
    where pair :: [Int] -> [Pos]
          pair []       = []
          pair (x:y:xs) = (x,y) : pair xs
          pair [e]      = error ("This should never happen! pair was called on: "++show e)
parse e = error ("This should never happen! parse was called on string not starting with s or b: "++e)

emptyBoard :: Board
emptyBoard = B 0 [] (0,0) (0,0)

putRow :: Int -> Int -> IO ()
putRow row size = do
    putStr ((if row < 10 then " " else "") ++ show row ++ " ")
    putStrLn (concat (replicate size ".  "))

draw :: Board -> IO ()
draw (B size pss _ _) = do
  cls
  putStr "   "
  sequence_ [putStr (show n ++ (if n < 9 then "  " else " ")) | n <- [1..size]]
  putStrLn ""
  sequence_ [putRow n size | n  <- [1..size]]
  putcells pss

putcells :: [Pos] -> IO ()
putcells pss = sequence_ [writeat (1 + 3*x, 1 + y) "X" | (x,y) <- pss]
remcells :: [Pos] -> IO ()
remcells pss = sequence_ [writeat (1 + 3*x, 1 + y) "." | (x,y) <- pss]

neighbs :: Size -> Pos -> [Pos]
neighbs s (x,y) = filter (\(i,j) -> 1 <= i && i <= s && 1 <= j &&  j <= s)
  [(x-1,y-1),(x,y-1),(x+1,y-1),(x-1,y),(x+1,y),(x-1,y+1),(x,y+1),(x+1,y+1)]

liveneighbs :: Size -> [Pos] -> Pos -> Int
liveneighbs s pss = length . filter (`elem` pss) . neighbs s

survivors :: Board -> [Pos]
survivors (B s pss surv _) = [p | p <- pss, liveneighbs s pss p `elem` [fst surv..snd surv]]

births :: Board -> [Pos]
births (B s pss _ birt) = [p | p <- nub (concatMap (neighbs s) pss),
                           p `notElem` pss,
                           liveneighbs s pss p `elem` [fst birt..snd birt]]
nextgen :: Board -> Board
nextgen (B s pss surv birt) = let b = B s pss surv birt in
  B s (survivors b ++ births b) surv birt

step :: Board -> IO Board
step (B s pss surv birt) = do
  let (B ns npss nsurv nbirt) = nextgen (B s pss surv birt)
  remcells (filter (`notElem` npss) pss)
  putcells (filter (`notElem` pss) npss)
  return (B ns npss nsurv nbirt)

data Command = Create Size | New Pos | Destroy Pos | Survival SRule | Birth BRule
  | Rules | Write String | Read String | Step | Live [Board] Int | Quit
  deriving (Eq, Show)

getInitialCommand :: IO Command
getInitialCommand = do
  c <- getCommand (-2)
  case c of
    (Create size)   -> return (Create size)
    (Read filename) -> return (Read filename)
    Quit            -> return Quit
    _               -> do
      putStrLn "You must create board first! (write \"c n\" where n is an integer)"
      getInitialCommand

getCommand :: Int -> IO Command
getCommand s = do
  resetPrompt s
  putStr "Enter command: "
  line <- getLine
  case line of
    ('c':r) -> if all isDigit ((concat . words) r) && length (words r) == 1
      && (read r :: Int) > 0 && (read r :: Int) < 100 then
        return (Create (read r :: Int))
      else invalid s r
    ('q':_) -> return Quit
    ('n':r) -> if all isDigit ((concat . words) r) && length (words r) == 2 then do
        let p = map read (words r) :: [Int]
        return (New (head p, p !! 1))
      else invalid s r
    ('d':r) -> if all isDigit ((concat . words) r) && length (words r) == 2 then do
        let p = map read (words r) :: [Int]
        return (Destroy (head p, p !! 1))
      else invalid s r
    ('s':r) -> if all isDigit ((concat . words) r) && length (words r) == 2 then do
        let n = map read (words r) :: [Int]
        return (Survival (head n, n !! 1))
      else invalid s r
    ('b':r) -> if all isDigit ((concat . words) r) && length (words r) == 2 then do
        let n = map read (words r) :: [Int]
        return (Birth (head n, n !! 1))
      else invalid s r
    ('?':_) -> return Rules
    ('l':r) -> if all isDigit ((concat . words) r) && length (words r) == 1
      && (read r :: Int) >= 1 then
        return (Live [] (read r :: Int))
      else invalid s r
    [] -> return Step
    ('r':' ':name) -> return (Read name)
    ('w':' ':name) -> return (Write name)
    e       -> do
      resetInfo s
      putStrLn ("Unsupported command: " ++ e)
      getCommand s

invalid :: Int -> String -> IO Command
invalid s r = do
  resetInfo s
  putStrLn ("Invalid input:" ++ r)
  getCommand s

resolve :: Board -> Command -> IO ()
resolve (B s pss surv birt) c = case c of
  (Create size) -> do
    let b = B size [] (2,3) (3,3)  -- defaults to conway's rules
    draw b
    mainloop b
  Quit -> return ()
  (New (x,y)) -> do
    resetInfo s
    if (x,y) `notElem` pss && 1 <= x && x <= s && 1 <= y && y <= s then do
      putcells [(x,y)]
      mainloop (B s ((x,y):pss) surv birt)
    else mainloop (B s pss surv birt)
  (Destroy (x,y)) -> do
    resetInfo s
    if (x,y) `elem` pss && 1 <= x && x <= s && 1 <= y && y <= s then do
      remcells [(x,y)]
      mainloop (B s (filter (/=(x,y)) pss) surv birt)
    else mainloop (B s pss surv birt)
  (Survival (x,y)) -> do
    resetInfo s
    if 0 <= x && x <= y then mainloop (B s pss (x,y) birt)
    else do
      putStr "Invalid survival rule!"
      mainloop (B s pss surv birt)
  (Birth (x,y)) -> do
    resetInfo s
    if 0 <= x && x <= y then mainloop (B s pss surv (x,y))
    else do
      putStr "Invalid birth rule!"
      mainloop (B s pss surv birt)
  Rules -> do
    resetInfo s
    putStr("Rules: s " ++ show surv ++ ", b " ++ show birt)
    mainloop (B s pss surv birt)
  Step -> do
    b <- step (B s pss surv birt)
    mainloop b
  (Live his x) -> if x == 0 then mainloop (B s pss surv birt)
    else do
      b <- step (B s pss surv birt)
      if his /= [] && b == head his then do
        resetInfo s
        putStrLn ("Reached stable state. Will not execute remaining " ++ show x ++ " generations.")
        mainloop b
      else do
        threadDelay 300000
        resolve b (Live (b:his) (x-1))
  (Read filename) -> do
    bs <- readFile filename
    let b = parse bs
    draw b
    mainloop b
  (Write filename) -> do
    let b = B s pss surv birt
    writeFile filename (show b)
    mainloop b
