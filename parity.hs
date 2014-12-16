module Main where

import Diagrams.Prelude
import Diagrams.Backend.SVG

type Bit = Int

xor :: Bit -> Bit -> Bit
xor a b | a == b = 0
        | a /= b = 1

type BitV  = [Bit]

bvi :: BitV -> BitV
bvi [] = []
bvi xs = (tail . scanl xor 0) xs

bsi :: BitV -> Bit
bsi = last . bvi

type BitM = [BitV]

-- Pariton
type Pariton = BitM
par xs = (takeWhile (/= xs) $ tail $ iterate bvi xs) ++ [xs]

type Diag = Diagram SVG R2

-- Black or white squares
bsquare :: Diag
bsquare = square 1 # fc black # lwG 0.01 # lc purple
wsquare :: Diag
wsquare = square 1 # fc white # lwG 0.01 # lc purple

-- Build a grid of white or black squares as a par
ppar :: Pariton -> Diag
ppar = foldl (===) mempty . map pvec
       where pvec = foldl (|||) mempty . map pelem
             pelem x | x == 1 = bsquare
                     | x == 0 = wsquare

-- geniton de taille n
selem n = 1:(replicate (n-1) 0)
gen n = par $ selem n

printDiag filepath = renderSVG filepath (Width 800)

main = do
    printDiag "parity.svg" $ ppar $ gen $ 2^4
