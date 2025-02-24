{-
-- EPITECH PROJECT, 2025
-- Wolfram
-- File description:
-- Main
-}

module Main where

import System.Environment (getArgs)
import Utils
import WolframData
import ArgsData
import Args
import Wolfram

main :: IO ()
main = do
    progArgs <- getArgs
    help progArgs
    case argsGetter progArgs newArgs of
        Just args ->
            if null progArgs || rule args == -1
            then usageWithRet 84
            else printWolfram args (drop (start args) (generateInfiniteWolfram
            (rule args) newWolfram))
        _ -> usageWithRet 84
