module Main where

import System.Environment (getArgs)

type Matrix a = [Row a]
type Row a = [a]
type Grid = Matrix Digit
type Digit = Char

digits :: [Char]
digits = ['1' .. '9']

blank :: Digit -> Bool
blank = (== '0')

solve :: Grid -> [Grid]
solve = filter valid . completions

completions :: Grid -> [Grid]
completions = expand . choices

type ChoicesGrid = Matrix [Digit]
choices :: Grid -> ChoicesGrid
choices grid = map (map choice) grid
                   where choice x = if blank x then digits else [x]

-- Cartesian product
cp :: [[a]] -> [[a]]
cp [] = [[]]
cp (xs:xss) = [x:ys | x <- xs, ys <- yss]
              where yss = cp xss

-- Expand: choices grid -> list of choices
expand :: ChoicesGrid -> [Grid]
expand = cp . map cp

-- Validate a grid
valid :: Grid -> Bool
valid g = all nodups (rows g) &&
          all nodups (cols g) &&
          all nodups (boxs3 g)

-- Check if there are duplicates in a list
nodups :: (Eq a) => [a] -> Bool
nodups [] = True
nodups (x:xs) = all (/= x) xs && nodups xs

-- Get the rows from a grid
rows :: Matrix a -> Matrix a
rows g = g

-- Columns
cols :: Matrix a -> Matrix a
cols [xs] = [[x] | x <- xs]
cols (xs:xss) = zipWith (:) xs (cols xss)

-- Boxes
boxs :: Int -> Matrix a -> Matrix a
boxs n = map ungroup . ungroup .
         map cols .
         groupn . map groupn
         where groupn = group n
boxs3 = boxs 3

group :: Int -> [a] -> [[a]]
group _ [] = []
group n xs = take n xs : group n (drop n xs)

ungroup :: [[a]] -> [a]
ungroup = concat

pruneRow :: Row [Digit] -> Row [Digit]
pruneRow r = map (remove singles) r
             where singles = [d | [d] <- r]

remove :: [Digit] -> [Digit] -> [Digit]
remove ds [x] = [x]
remove ds xs  = filter (`notElem` ds) xs

prune :: ChoicesGrid -> ChoicesGrid
pruneBy f = f . map pruneRow . f
prune = pruneBy rows . pruneBy cols . pruneBy boxs3

solve' :: Grid -> [Grid]
solve' = filter valid . expand . prune . choices

many :: (Eq a) => (a -> a) -> a -> a
many f a = if a == b then a
           else many f b
           where b = f a

solve'' = filter valid . expand . many prune . choices

--expand' :: ChoicesGrid -> [ChoicesGrid]

single :: [a] -> Bool
single [_] = True
single _ = False

s2g :: String -> Grid
s2g = group 9

main = do
    [f] <- getArgs
    g:gs <- fmap lines $ readFile f
    print $ head $ solve'' . s2g $ g
