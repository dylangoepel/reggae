module Main where

import System.Environment
import Recognizer
import Regex

main :: IO ()
main = do
    args <- getArgs
    if length args < 2 then
        error "Invalid usage."
    else
        matchPattern (compileRegex (head args)) (args !! 1)

matchPattern :: Maybe (Recognizer Char) -> String -> IO ()
matchPattern Nothing s = fail "Invalid regular expression."
matchPattern (Just r) s = case recognize r s of
    Nothing -> error "No match."
    Just tail -> putStrLn tail
