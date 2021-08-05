module Regex where

import Parser 
import Recognizer

charRange :: Parser String
charRange = pAnd enumFromTo (pBut "[^-" <<< pToken "-") (pBut "-]")

alphabet :: Parser String
alphabet = pRepeat $ pOr charRange (pNonempty $ pUntil "^[")

bracket :: Parser (Recognizer Char)
bracket = pOr
            (pMap NoneOf $ pToken "[^" >>> alphabet <<< pToken "]")
            (pMap OneOf $ pToken "[" >>> alphabet <<< pToken "]")

asterisk :: Parser (Recognizer Char)
asterisk = pMap While $ subPattern <<< pToken "*"

plus :: Parser (Recognizer Char)
plus = pMap (\x -> Sequence [x, While x]) $ subPattern <<< pToken "+"

alternative :: Parser (Recognizer Char)
alternative = pAnd Or subPattern (pToken "|" >>> subPattern)

subPattern :: Parser (Recognizer Char)
subPattern = plus `pOr` asterisk `pOr` bracket `pOr`
    (pToken "(" >>> regexP <<< pToken ")")

regexP :: Parser (Recognizer Char)
regexP = pRepeat $ pOr alternative subPattern

compileRegex :: String -> Maybe (Recognizer Char)
compileRegex s = fmap fst (regexP $ escape '\\' s)

tryRegex :: String -> String -> Maybe String
tryRegex s = case compileRegex s of
    Nothing -> error ""
    Just r  -> recognize r
