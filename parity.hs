module Main where

import Diagrams.Prelude
import Diagrams.Backend.SVG

xor a b | a == b = 0
        | a /= b = 1

bvi [] = []
bvi xs = (tail . scanl xor 0) xs

bsi = last . bvi

-- Pariton des xs
par xs = (takeWhile (/= xs) $ tail $ iterate bvi xs) ++ [xs]

type Diag = Diagram SVG R2

-- Black or white squares
bsquare = square 1 # fc black # lwG 0.01 # lc purple
wsquare = square 1 # fc white # lwG 0.01 # lc purple

-- Build a grid of white or black squares as a par
ppar = foldl (===) mempty . map pvec . par
       where pvec = foldl (|||) mempty . map pelem
             pelem x | x == 1 = bsquare
                     | x == 0 = wsquare

-- 10000 de taille n
gen n = [1] ++ (take n $ repeat 0)

printDiag filepath = renderSVG filepath (Width 800)

main = do
    printDiag "parity.svg" (ppar xs)
    where xs = gen $ 2^6
