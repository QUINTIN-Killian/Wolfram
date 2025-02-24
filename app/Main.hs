{-
-- EPITECH PROJECT, 2025
-- Wolfram
-- File description:
-- Main
-}

module Main where

import Data.Maybe
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
    let (Args r s l w m err) = exploreArgs progArgs newArgs
    if null progArgs || err || r == Nothing
    then usageWithRet 84
    else printWolfram (setArgs (Args r s l w m err)) (drop (fromMaybe 0 s) (
        generateInfiniteWolfram (fromJust r) newWolfram))
