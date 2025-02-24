{-
-- EPITECH PROJECT, 2025
-- Wolfram
-- File description:
-- ArgsData
-}

module ArgsData (Args(..), newArgs) where

data Args = Args {
    rule :: Maybe Int,
    start :: Maybe Int,
    nbLines :: Maybe Int,
    window :: Maybe Int,
    move :: Maybe Int,
    err :: Bool
}

instance Show Args where
    show (Args rule start nbLines window move err) = (
        "rule : " ++ show rule ++ 
        "\nstart : " ++ show start ++ 
        "\nlines : " ++ show nbLines ++ 
        "\nwindow : " ++ show window ++ 
        "\nmove : " ++ show move ++ 
        "\nerr : " ++ show err
        )

newArgs :: Args
newArgs = (Args Nothing Nothing Nothing Nothing Nothing False)
