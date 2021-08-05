module Recognizer where

import Data.Bifunctor (first)

data Recognizer a = OneOf [a]
    | NoneOf [a]
    | Exact a
    | Or (Recognizer a) (Recognizer a)
    | Sequence [Recognizer a]
    | While (Recognizer a)
    | Count (Recognizer a) Integer
    | CountRange (Recognizer a) Integer Integer deriving (Show, Eq)

instance Semigroup (Recognizer a) where
    (Sequence x) <> (Sequence y) = Sequence (x ++ y)
    p <> q = Sequence [p, q]

instance Monoid (Recognizer a) where
    mempty = Sequence []

recognize :: Eq a => Recognizer a -> [a] -> Maybe ([a], [a])
recognize (OneOf alphabet) [] = Nothing
recognize (OneOf alphabet) (x:xs)
    | x `elem` alphabet = Just ([x], xs)
    | otherwise = Nothing
recognize (NoneOf alphabet) [] = Nothing
recognize (NoneOf alphabet) (x:xs)
    | x `elem` alphabet = Nothing
    | otherwise = Just ([x], xs)
recognize (Or a b) xs = case recognize a xs of
    Nothing -> recognize b xs
    Just xs -> Just xs
recognize (Sequence (a:as)) xs = do
    (r, t) <- recognize a xs
    (rs, ts) <- recognize (Sequence as) t
    return (r ++ rs, ts)
recognize (Sequence []) xs = Just ([], xs)
recognize (While a) xs = case recognize a xs of
    Nothing -> Just ([], xs)
    Just (rt, tr) -> first (rt ++) <$> recognize (While a) tr
recognize (Count a 0) xs = Just ([], xs)
recognize (Count a c) xs = do
    (r, t) <- recognize a xs
    (rs, ts) <- recognize (Count a (c - 1)) t
    return (r ++ rs, ts)
recognize (CountRange a 0 0) xs = Just ([], xs)
recognize (CountRange a 0 n) xs
    = case recognize a xs of
        Nothing -> Just ([], xs)
        Just (re, tr) -> first (re++)
            <$> recognize (CountRange a 0 (n - 1)) tr
recognize (CountRange a n m) xs
    = recognize (Sequence [Count a n, CountRange a 0 (m - n)]) xs
recognize (Exact a) (x:xs)
    | x == a = Just ([a], xs)
    | otherwise = Nothing
recognize (Exact a) [] = Nothing

find :: Eq a => Recognizer a -> [a] -> Maybe [a]
find r (x:xs) = case recognize r (x:xs) of
    Nothing -> find r xs
    Just (res, tr) -> Just res
find r [] = fst <$> recognize r []

findAll :: Eq a => Recognizer a -> [a] -> [[a]]
findAll r (x:xs) = case recognize r (x:xs) of
    Nothing -> findAll r xs
    Just (res, tr) -> res : findAll r tr
findAll _ [] = []
