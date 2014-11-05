module Main where

import Data.Char
import Data.List
import System.Console.GetOpt
import System.Environment ( getArgs )
import Data.Maybe ( fromMaybe )

-- Split a string by splitting it along a predicate
split :: (Char -> Bool) -> String -> [String]
split f s = case dropWhile f s of
                    "" -> []
                    s' -> w : split f s''
                              where (w, s'') = break f s'

-- Count the number of occurences of the first element of the list and return it as a list of pairs.
countRuns [] = []
countRuns (w:ws) = (1 + length us, w) : countRuns vs
                     where (us, vs) = span (== w) ws

-- Token abstraction
type Token = String

-- Split a string into tokens, which are separated by space or punctuation.
tokens :: String -> [Token]
tokens = split (\ c -> isSpace c || isPunctuation c)

-- Count the tokens in a string, returning the list of pairs (number of occurences, token) for each
-- token. The tokens are listed in reverse order of occurence.
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

-- Possible command line flags
data Flag = Output String | Input String | Help deriving Show
options :: [OptDescr Flag]
options =
    [ Option ['o'] ["output"] (OptArg (Output . fromMaybe "stdout") "FILE") "Output file"
    , Option ['i'] ["input" ] (ReqArg Input "FILE"                        ) "Input file"
    , Option ['h'] ["help"  ] (NoArg  Help                                ) "Show help"
    ]

-- Parse command line options
cliOpts :: [String] -> IO ([Flag], [String])
cliOpts argv = case getOpt Permute options argv of
                    (o, n, []  ) -> return (o, n)
                    (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
                                    where header = "Usage: intro [OPTION...]"

-- Main program
main = do args <- getArgs
          (opts, n) <- cliOpts args
          print opts
