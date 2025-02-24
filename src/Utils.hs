{-
-- EPITECH PROJECT, 2025
-- Wolfram
-- File description:
-- Utils
-}

module Utils (isInt, isIntInRange, isNeg, isPos, usageWithRet) where

import System.Exit (exitWith, ExitCode (ExitFailure, ExitSuccess))

isNbr :: String -> Bool
isNbr [] = True
isNbr (x : xs)
    | x >= '0' && x <= '9' = isNbr xs
    | otherwise = False

checkNeg :: String -> Bool
checkNeg [] = False
checkNeg ('-' : xs) = isNbr xs
checkNeg str = isNbr str

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

usageWithRet :: Int -> IO ()
usageWithRet ret =
    putStrLn ("./wolfram --rule n (--start n) (--lines n) (--window n) " ++
    "(--move n)") >>
    putStrLn "--rule : rule to display" >>
    putStrLn "--start : starting line" >>
    putStrLn "--lines : number of lines" >>
    putStrLn "--window : line width" >>
    putStrLn "--move : a translation to apply" >>
    exitWith (if ret == 0 then ExitSuccess else ExitFailure ret)
