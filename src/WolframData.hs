{-
-- EPITECH PROJECT, 2025
-- Wolfram
-- File description:
-- WolframData
-}

module WolframData (State(..), Wolfram(..), showStateList, newWolfram) where

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
    show wolfram = showStateList (leftList wolfram) ++ showStateList (rightList
        wolfram)

newWolfram :: Wolfram
newWolfram = (Wolfram (repeat Dead) (Alive : repeat Dead))
