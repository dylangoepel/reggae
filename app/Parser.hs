module Parser where

import Data.Bifunctor (first)

data Escaped a = NoEscape a | Escape a deriving (Show, Eq)

escape :: Eq a => a -> [a] -> [Escaped a]
escape esc (x:y:xs)
    | x == esc = Escape y : escape esc xs
    | otherwise = NoEscape x : escape esc (y:xs)
escape esc xs = map NoEscape xs

lift :: Escaped a -> a
lift (NoEscape a) = a
lift (Escape a) = a

type Parser a = [Escaped Char] -> Maybe (a, [Escaped Char])

pMap :: (a -> b) -> Parser a -> Parser b
pMap f p = fmap (first f) . p 

pOr :: Parser a -> Parser a -> Parser a
pOr p q s = case p s of
    Nothing -> q s
    x -> x

pAnd :: (b -> a -> c) -> Parser b -> Parser a -> Parser c
pAnd f p q s = do
    (rq, tq) <- q s
    (rp, tp) <- p tq
    return (f rp rq, tp)

pNonempty :: (Monoid a, Eq a) => Parser a -> Parser a
pNonempty p t = case p t of
    Nothing -> Nothing
    Just (x, xs) -> if x == mempty then
            Nothing
        else
            Just (x, xs)

pEmpty :: Monoid a => Parser a
pEmpty s = Just (mempty, s)

pRepeat :: Monoid a => Parser a -> Parser a
pRepeat p = pOr (pAnd (<>) (pRepeat p) p) pEmpty

pToken :: String -> Parser ()
pToken [] s = Just ((), s)
pToken _ [] = Nothing
pToken ts xs
    | last xs == NoEscape (last ts) = pToken (init ts) (init xs)
    | otherwise = Nothing

pAny :: String -> Parser Char
pAny a [] = Nothing
pAny a xs
    | last xs `elem` map NoEscape a = Just (lift $ last xs, init xs)
    | otherwise = Nothing

pBut :: String -> Parser Char
pBut a [] = Nothing
pBut a xs
    | last xs `elem` map NoEscape a = Nothing
    | otherwise = Just (lift $ last xs, init xs)

pWord :: String -> Parser String
pWord = pRepeat . (pMap (: []) . pAny)

pUntil :: String -> Parser String
pUntil = pRepeat . (pMap (: []) . pBut)

(>>>) :: Parser a -> Parser b -> Parser b
p >>> q = pAnd (const id) p q

(<<<) :: Parser a -> Parser b -> Parser a
p <<< q = pAnd const p q
