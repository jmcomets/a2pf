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

-- Split a string into tokens, which are separated by space or punctuation.
tokens = split (\ c -> isSpace c || isPunctuation c)

-- Count the tokens in a string, returning the list of pairs (number of
-- occurences, token) for each token. The tokens are listed in reverse order of
-- occurence.
countTokens :: String -> [(Int, String)]
countTokens = countRuns . sort . tokens . map toLower

-- Take the n most occurences of tokens in String s (see countTokens).
commonWords n s = take n $ reverse $ countTokens s

-- Main program
main =
  do putStrLn "Input text?"
     text <- getLine
     putStrLn "How many words?"
     n <- getLine
     putStrLn (intercalate "\n" (map (\ (x, y) -> (show x) ++ ": " ++ y) (commonWords (read n) text)))
