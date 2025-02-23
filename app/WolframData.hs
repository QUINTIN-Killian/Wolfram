{-
-- EPITECH PROJECT, 2025
-- Wolfram
-- File description:
-- WolframData
-}

module WolframData (State(..), Wolfram(..), showStateList, newLeftRow,
newRightRow, newWolfram) where

data State = Dead | Alive deriving Eq

instance Show State where
    show Dead = " "
    show Alive = "*"

data Wolfram = Wolfram {
    leftList :: [State],
    rightList :: [State]
}

showStateList :: [State] -> String
showStateList [] = ""
showStateList (x:xs) = show x ++ showStateList xs

instance Show Wolfram where
    show (Wolfram left right) = showStateList left ++ showStateList right

newLeftRow :: [State]
newLeftRow = repeat Dead

newRightRow :: [State]
newRightRow = Alive : repeat Dead

newWolfram :: Wolfram
newWolfram = (Wolfram newLeftRow newRightRow)
