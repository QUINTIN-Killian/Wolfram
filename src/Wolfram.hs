{-
-- EPITECH PROJECT, 2025
-- Wolfram
-- File description:
-- Wolfram
-}

module Wolfram (getState, getRightList, getLeftList, generateInfiniteWolfram,
printWolfram) where

import Data.Bits
import ArgsData
import WolframData

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

getRightList :: Int -> [State] -> State -> [State]
getRightList ruleNb (x1 : x2 : xs) other =
    (getState ruleNb (other, x1, x2)) : (getRightList ruleNb (x2 : xs) x1)
getRightList _ _ _ = []

getLeftList :: Int -> [State] -> State -> [State]
getLeftList ruleNb (x1 : x2 : xs) other =
    (getState ruleNb (x2, x1, other)) : (getLeftList ruleNb (x2 : xs) x1)
getLeftList _ _ _ = []

generateInfiniteWolfram :: Int -> Wolfram -> [Wolfram]
generateInfiniteWolfram ruleNb wolfram@(Wolfram (lHead : lRest)
    (rHead : rRest)) = wolfram : generateInfiniteWolfram ruleNb (Wolfram
    (getLeftList ruleNb (lHead : lRest) rHead) (getRightList ruleNb (rHead :
    rRest) lHead))
generateInfiniteWolfram _ _ = []

printWolfram :: Args -> [Wolfram] -> IO ()
printWolfram (Args _ _ 0 _ _) _ = return ()
printWolfram args@(Args r s _ w m) (wolfram : rest) = putStr (showStateList
    (reverse (take ((window args) `div` 2) (leftList wolfram)))) >> putStrLn (
    showStateList (take (((window args) `div` 2) + ((window args) `mod` 2))
    (rightList wolfram))) >> printWolfram (Args r s ((nbLines args) - 1) w m)
    rest
printWolfram _ _ = return ()
