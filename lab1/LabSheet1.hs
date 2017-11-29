import Data.Char

square :: Int -> Int
square x = x * x

pyth (x, y) = square x + square y

pyth' :: Int -> Int -> Int
pyth' x y = square x + square y

isTriple :: Int -> Int -> Int -> Bool
isTriple x y z = if x > 0 && y > 0 && z > 0  then pyth(x, y) == square z else False

isTripleAny :: Int -> Int -> Int -> Bool
isTripleAny x y z 
            =   if x > 0 && y > 0 && z > 0  
                then pyth(x, y) == square z || pyth(x, z) == square y || pyth(y, z) == square x 
                else False

halfEvens :: [ Int ] -> [ Int ]
halfEvens x = [if mod x 2 == 1 then x else div x 2 | x <- x]

inRange :: Int -> Int -> [Int] -> [Int]
inRange a b x = [x | x <- x, x >= a, x <= b]

countPositives :: [ Int ] -> Int
countPositives x = length [x| x <- x, x > 0]

capitalised :: String -> String
capitalised [] = ""
capitalised x = head [toUpper x| x <- x] : tail [toLower x| x <- x]

capitalised'::String->String
capitalised' [] = ""
capitalised' (x:xs) = toUpper x:[toLower c|c<-xs]

titleNext :: [String] -> [String]
titleNext (x:xs) 
    | length x > 4 = capitalised x : titleN
    | otherwise = [toLower s| s <- x] : titleN
    where titleN = titleNext xs
titleNext []     = []

title :: [String] -> [String]
title (x:xs) = capitalised x : xs


