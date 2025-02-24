{-
-- EPITECH PROJECT, 2025
-- Wolfram
-- File description:
-- ArgsData
-}

module ArgsData (Args(..), newArgs) where

data Args = Args {
    rule :: Int,
    start :: Int,
    nbLines :: Int,
    window :: Int,
    move :: Int
}

instance Show Args where
    show args =
        "rule : " ++ show (rule args) ++
        "\nstart : " ++ show (start args) ++
        "\nlines : " ++ show (nbLines args) ++
        "\nwindow : " ++ show (window args) ++
        "\nmove : " ++ show (move args)

newArgs :: Maybe Args
newArgs = Just (Args (-1) 0 (-1) 80 0)
