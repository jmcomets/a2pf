module Main where

data Op = Add | Sub | Mul | Div
          deriving (Show)
ops = [Add, Sub, Mul, Div]

valid :: Op -> Int -> Int -> Bool
valid op x y = case op of
                    Add -> x >= 0 && y >= 0
                    Mul -> (x >= 0 && y >= 0) || (x <= 0 && y <= 0)
                    Sub -> x >= y
                    Div -> y /= 0 && x `mod` y == 0

apply :: Op -> Int -> Int -> Int
apply op x y = case op of
                    Add -> x + y
                    Sub -> x - y
                    Mul -> x * y
                    Div -> x `div` y

data Expr = Val Int | App Op Expr Expr
            deriving (Show)

values :: Expr -> [Int]
values (Val x)     = [x]
values (App _ x y) = values x ++ values y

eval :: Expr -> [Int]
eval (Val x) = [x | x >= 0]
eval (App op x y) = [apply op a b | a <- eval x,
                                    b <- eval y,
                                    valid op a b]

choices :: [a] -> [[a]]
choices [] = [[]]
choices (x:xs) = cs ++ (concat . map (interleave x)) cs
               where cs = choices xs

interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys):map (y:) (interleave x ys)

solution :: Expr -> [Int] -> Int -> Bool
solution e cs x = elem (values e) (choices cs) && eval e == [x]

split :: [a] -> [([a], [a])]
split [] = []
split [_] = []
split (x:xs) = ([x], xs):[(x:ls, rs) | (ls, rs) <- split xs]

exprs :: [Int] -> [Expr]
exprs [x] = [Val x]
exprs ns = [e | (xs, ys) <- split ns,
                    x <- exprs xs,
                    y <- exprs ys,
                    e <- combine x y]

combine :: Expr -> Expr -> [Expr]
combine x y = [App o x y | o <- ops]

solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns,
                      e <- exprs ns',
                      eval e == [n]]

main = do
    print $ show $ length (solutions ss x)
    where ss = [1, 2, 3, 4, 8]
          x = 4
