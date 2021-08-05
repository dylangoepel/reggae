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

countRange :: Parser (Recognizer Char)
countRange = pAnd (\x (a, b) -> CountRange x a b)
    subPattern
    (pAnd (\x y -> (read x, read y))
          (pToken "{" >>> pWord "1234567890" <<< pToken ",")
                         (pWord "1234567890" <<< pToken "}"))

countExact :: Parser (Recognizer Char)
countExact = pAnd Count
    subPattern
    (pMap read (pToken "{" >>> pWord "0123456789" <<< pToken "}"))

dot :: Parser (Recognizer Char)
dot = pMap (const $ NoneOf "") $ pToken "."

subPattern :: Parser (Recognizer Char)
subPattern = countExact `pOr`
    countRange `pOr`
    plus `pOr`
    asterisk `pOr`
    bracket `pOr`
    (pToken "(" >>> regexP <<< pToken ")") `pOr`
    dot `pOr`
    pMap Exact (pBut "()[]{}+*")


regexP :: Parser (Recognizer Char)
regexP = pRepeat $ pOr alternative subPattern

compileRegex :: String -> Maybe (Recognizer Char)
compileRegex s = fmap fst (regexP $ escape '\\' s)
