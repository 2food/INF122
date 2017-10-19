-- Tormod Mathiesen
module Oblig1 where
    import Data.Char
    import Data.Tuple

    data Ast = Nr Int | Sum Ast Ast | Mul Ast Ast | Min Ast | If Ast Ast Ast | Let String Ast Ast | Var String
        deriving (Eq, Show)

    astList :: String -> [Ast] -- returns a list of parsed Asts in current layer of parseExpr
    astList []  = []
    astList str = let (a, s) = parseExpr str in a:(astList s)
    restList :: String -> [String] -- returns the unparsed remaining String for every Ast in current layer of parseExpr
    restList []  = []
    restList str = let (a, s) = parseExpr str in s:(restList s)

    parseExpr :: String -> (Ast, String)
    parseExpr (' ':s)           = parseExpr s
    parseExpr ('=':s)           = parseExpr s
    parseExpr ('i':'n':s)       = parseExpr s
    parseExpr ('+':s)           = ((Sum (astList s !! 0) (astList s !! 1)), restList s !! 1)
    parseExpr ('*':s)           = ((Mul (astList s !! 0) (astList s !! 1)), restList s !! 1)
    parseExpr ('-':s)           = (Min (astList s !! 0), restList s !! 0)
    parseExpr ('i':'f': s)      = (If (astList s !! 0) (astList s !! 1) (astList s !! 2), restList s !! 2)
    parseExpr ('l':'e':'t':s)   = (Let ((\(Var x) -> x) (astList s !! 0)) (astList s !! 1) (astList s !! 2), restList s !! 2)
    parseExpr (x:s) | isDigit x = (Nr (read (takeWhile isDigit (x:s)) :: Int), dropWhile isDigit s)
                    | isUpper x = (Var (x:[]), s)
                    | otherwise = error ("Illegal character: " ++ x:[])

    parse :: String -> Ast
    parse str = let (ast, rest) = parseExpr str in 
        if rest == [] 
            then ast 
            else error ("Bad input string caused following string to remain unparsed: "++rest)

    find :: String -> [(String,a)] -> a
    find x []                       = error ("Variable ("++x++") is undeclared.")
    find x ((k,v):rest) | x == k    = v
                        | otherwise = find x rest

    folde :: (Int -> a) -> (a -> a -> a) -> (a -> a -> a) -> (a -> a) -> (a -> a -> a -> a) ->[(String,a)] -> Ast -> a
    folde n sum mu mi fi env (Nr x)      = n x
    folde n sum mu mi fi env (Sum x y)   = sum (folde n sum mu mi fi env x) (folde n sum mu mi fi env y)
    folde n sum mu mi fi env (Mul x y)   = mu (folde n sum mu mi fi env x) (folde n sum mu mi fi env y)
    folde n sum mu mi fi env (Min x)     = mi (folde n sum mu mi fi env x)
    folde n sum mu mi fi env (If x y z)  = fi (folde n sum mu mi fi env x) (folde n sum mu mi fi env y) (folde n sum mu mi fi env z)
    folde n sum mu mi fi env (Let x y z) = let newenv = (x,(folde n sum mu mi fi env y)):env in (folde n sum mu mi fi newenv z)
    folde n sum mu mi fi env (Var x)     = find x env

    evi :: String -> Int
    evi = folde id (+) (*) (\x -> -x) (\x y z -> if x==0 then y else z) [] . parse

    evb :: String -> Bool
    evb = folde (\x -> mod x 2 /= 0) (||) (&&) (not) (\x y z -> if x then y else z) [] . parse
