{-
-- EPITECH PROJECT, 2025
-- Wolfram
-- File description:
-- Utils
-}

module Utils (isInt, isIntInRange, isNeg, isPos) where

isNbr :: String -> Bool
isNbr [] = True
isNbr (x:xs)
    | x >= '0' && x <= '9' = isNbr xs
    | otherwise = False

checkNeg :: String -> Bool
checkNeg [] = False
checkNeg l
    | head l == '-' = isNbr (tail l)
    | otherwise = isNbr l

isInt :: String -> Bool
isInt [] = False
isInt l
    | checkNeg l = True
    | otherwise = False

isIntInRange :: Int -> Int -> Int -> Bool
isIntInRange nb x y
    | nb >= x && nb <= y = True
    | otherwise = False

isNeg :: Int -> Bool
isNeg n
    | n < 0 = True
    | otherwise = False

isPos :: Int -> Bool
isPos n
    | n >= 0 = True
    | otherwise = False
