module Main where

import System.Environment
import Recognizer
import Regex

main :: IO ()
main = do
    args <- getArgs
    if length args < 2 then
        ioError (userError "Please supply a regex and a file path")
    else
        matchPattern (compileRegex (head args)) $ args !! 1

matchPattern :: Maybe (Recognizer Char) -> String -> IO ()
matchPattern Nothing s = fail "Invalid regular expression."
matchPattern (Just r) s
    = readFile s >>=
        foldl (>>) (return ())
            . map putStrLn . findAll r
