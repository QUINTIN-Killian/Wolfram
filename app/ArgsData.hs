{-
-- EPITECH PROJECT, 2025
-- Wolfram
-- File description:
-- ArgsData
-}

module ArgsData (Args(..), newArgs) where

data Args = Args {
    r :: Maybe Int,
    s :: Maybe Int,
    l :: Maybe Int,
    w :: Maybe Int,
    m :: Maybe Int,
    err :: Bool
}

instance Show Args where
    show (Args r s l w m err) = "rule : " ++ show r ++ "\nstart : " ++ show s ++ "\nlines : " ++ show l ++ "\nwindow : " ++ show w ++ "\nmove : " ++ show m ++ "\nerr : " ++ show err

newArgs :: Args
newArgs = (Args Nothing Nothing Nothing Nothing Nothing False)
