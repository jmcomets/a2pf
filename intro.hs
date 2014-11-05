module Main where

import Data.Char
import Data.List

-- Split a string by splitting it along a predicate
split :: (Char -> Bool) -> String -> [String]
split f s = case dropWhile f s of
                    "" -> []
                    s' -> w : split f s''
                              where (w, s'') = break f s'

-- Count the number of occurences of the first element of the list and return
-- it as a list of pairs.
countRuns [] = []
countRuns (w:ws) = (1 + length us, w) : countRuns vs
                     where (us, vs) = span (== w) ws

type Token = String

-- Split a string into tokens, which are separated by space or punctuation.
tokens :: String -> [Token]
tokens = split (\ c -> isSpace c || isPunctuation c)

-- Count the tokens in a string, returning the list of pairs (number of
-- occurences, token) for each token. The tokens are listed in reverse order of
-- occurence.
countTokens :: String -> [(Int, Token)]
countTokens = countRuns . sort . tokens . map toLower

-- Show a single token count
showTokenCount :: (Int, Token) -> String
showTokenCount (n, t) = t ++ "," ++ show n

-- Take the n most occurences of tokens in String s (see countTokens).
commonWords 0 s = countTokens s
commonWords n s = take n $ reverse $ commonWords 0 s

-- Show all entries of a commonWords return
showCommonWords :: Int -> String -> String
showCommonWords n = concat . map showTokenCount . take n . countTokens

-- Main program
main =
  do putStrLn "Input file? "
     inFile <- getLine
     fileContents <- readFile inFile
     putStrLn "Output file? "
     outFile <- getLine
     putStrLn "Word limit? (0: infinity) "
     n <- getLine
     writeFile outFile $ showCommonWords (read n) fileContents
     putStrLn $ "Output written to " ++ outFile
