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
    let (Args rule start nbLines window move err) =
            exploreArgs progArgs newArgs
    if null progArgs || err || rule == Nothing
    then usageWithRet 84
    else printWolfram (setArgs (Args rule start nbLines window move err))
        (drop (fromMaybe 0 start) (generateInfiniteWolfram (fromJust rule) 
        newWolfram))
