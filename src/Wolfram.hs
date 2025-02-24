{-
-- EPITECH PROJECT, 2025
-- Wolfram
-- File description:
-- Wolfram
-}

module Wolfram (getState, getRightList, getLeftList, generateInfiniteWolfram,
printWolfram) where

import Data.Maybe
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
getRightList ruleNb (x1:x2:xs) other =
    (getState ruleNb (other, x1, x2)) : (getRightList ruleNb (x2:xs) x1)

getLeftList :: Int -> [State] -> State -> [State]
getLeftList ruleNb (x1:x2:xs) other =
    (getState ruleNb (x2, x1, other)) : (getLeftList ruleNb (x2:xs) x1)

generateInfiniteWolfram :: Int -> Wolfram -> [Wolfram]
generateInfiniteWolfram ruleNb wolfram@(Wolfram left right) =
    wolfram : generateInfiniteWolfram ruleNb (Wolfram (getLeftList ruleNb left
    (head right)) (getRightList ruleNb right (head left)))

printWolfram :: Args -> [Wolfram] -> IO ()
printWolfram (Args r s (Just 0) w m err) _ = return ()
printWolfram (Args r s l w m err) ((Wolfram left right):xs) =
    putStr (showStateList (reverse (take ((fromJust w) `div` 2) left))) >>
    putStrLn (showStateList (take (((fromJust w) `div` 2) + ((fromJust w) `mod`
    2)) right)) >>
    printWolfram (Args r s (Just ((fromJust l) - 1)) w m err) xs
