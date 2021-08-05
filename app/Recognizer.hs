module Recognizer where

data Recognizer a = OneOf [a]
    | NoneOf [a]
    | Exact a
    | Or (Recognizer a) (Recognizer a)
    | Sequence [Recognizer a]
    | While (Recognizer a)
    | Count (Recognizer a) Integer deriving (Show, Eq)

instance Semigroup (Recognizer a) where
    (Sequence x) <> (Sequence y) = Sequence (x ++ y)
    p <> q = Sequence [p, q]

instance Monoid (Recognizer a) where
    mempty = Sequence []

recognize :: Eq a => Recognizer a -> [a] -> Maybe [a]
recognize (OneOf alphabet) [] = Nothing
recognize (OneOf alphabet) (x:xs)
    | x `elem` alphabet = Just xs
    | otherwise = Nothing
recognize (NoneOf alphabet) [] = Nothing
recognize (NoneOf alphabet) (x:xs)
    | x `elem` alphabet = Nothing
    | otherwise = Just xs
recognize (Or a b) xs = case recognize a xs of
    Nothing -> recognize b xs
    Just xs -> Just xs
recognize (Sequence (a:as)) xs = recognize a xs >>= recognize (Sequence as)
recognize (Sequence []) xs = Just xs
recognize (While a) xs = case recognize a xs of
    Nothing -> Just xs
    Just tr -> recognize (While a) tr
recognize (Count a 0) xs = Just xs
recognize (Count a c) xs = recognize a xs >>= recognize (Count a (c - 1))
recognize (Exact a) (x:xs)
    | x == a = Just xs
    | otherwise = Nothing
recognize (Exact a) [] = Nothing
