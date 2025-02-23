{-
-- EPITECH PROJECT, 2025
-- Wolfram
-- File description:
-- Main
-}

module Main where

import Data.Bits
import System.Environment (getArgs)
import Utils
import WolframData
import ArgsData
import Args

getBit :: Int -> Int -> Bool
getBit nb index = testBit nb index

castState :: Bool -> State
castState True = Alive
castState _ = Dead

getState :: Int -> (State, State, State) -> State
getState ruleNb (Dead, Dead, Dead) = castState (getBit ruleNb 0)
getState ruleNb (Dead, Dead, Alive) = castState (getBit ruleNb 1)
getState ruleNb (Dead, Alive, Dead) = castState (getBit ruleNb 2)
getState ruleNb (Dead, Alive, Alive) = castState (getBit ruleNb 3)
getState ruleNb (Alive, Dead, Dead) = castState (getBit ruleNb 4)
getState ruleNb (Alive, Dead, Alive) = castState (getBit ruleNb 5)
getState ruleNb (Alive, Alive, Dead) = castState (getBit ruleNb 6)
getState ruleNb (Alive, Alive, Alive) = castState (getBit ruleNb 7)

getRow :: Int -> [State] -> [State]
getRow ruleNb (x1:x2:x3:xs) = ((getState ruleNb (x1, x2, x3)) :
    (getRow ruleNb (x2:x3:xs)))
getRow _ _ = []

getCenter :: Int -> Wolfram -> State
getCenter ruleNb (Wolfram (x:xs) (y1:y2:ys)) = getState ruleNb (x, y1, y2)

main :: IO ()
main = do
    progArgs <- getArgs
    help progArgs
    let (Args r s l w m err) = exploreArgs progArgs newArgs
    if null progArgs || err || r == Nothing
    then usageWithRet 84
    else putStrLn (show (setArgs (Args r s l w m err)))
    -- putStrLn (showStateList (reverse (take 5 (getRow 110 left))))
    -- putStrLn (show (getCenter 110 (Wolfram left right)))
    -- putStrLn (showStateList (take 5 (getRow 110 right)))
    -- where (Wolfram left right) = newWolfram
