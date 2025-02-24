{-
-- EPITECH PROJECT, 2025
-- Wolfram
-- File description:
-- Setters
-}

module Args (argsGetter, help) where

import ArgsData
import Utils

setRule :: Maybe Args -> String -> Maybe Args
setRule Nothing _ = Nothing
setRule _ [] = Nothing
setRule (Just (Args _ s l w m)) val =
    if not (isInt val && isIntInRange (read val) 0 255)
    then Nothing
    else Just (Args (read val) s l w m)

setStart :: Maybe Args -> String -> Maybe Args
setStart Nothing _ = Nothing
setStart _ [] = Nothing
setStart (Just (Args r _ l w m)) val =
    if not (isInt val && isPos (read val))
    then Nothing
    else Just (Args r (read val) l w m)

setLines :: Maybe Args -> String -> Maybe Args
setLines Nothing _ = Nothing
setLines _ [] = Nothing
setLines (Just (Args r s _ w m)) val =
    if not (isInt val && isPos (read val))
    then Nothing
    else Just (Args r s (read val) w m)

setWindow :: Maybe Args -> String -> Maybe Args
setWindow Nothing _ = Nothing
setWindow _ [] = Nothing
setWindow (Just (Args r s l _ m)) val =
    if not (isInt val && isPos (read val))
    then Nothing
    else Just (Args r s l (read val) m)

setMove :: Maybe Args -> String -> Maybe Args
setMove Nothing _ = Nothing
setMove _ [] = Nothing
setMove (Just (Args r s l w _)) val =
    if not (isInt val)
    then Nothing
    else Just (Args r s l w (read val))

argsGetter :: [String] -> Maybe Args -> Maybe Args
argsGetter _ Nothing = Nothing
argsGetter [] args = args
argsGetter (_ : []) _ = Nothing
argsGetter ("--rule" : x2 : xs) args = argsGetter xs (setRule args x2)
argsGetter ("--start" : x2 : xs) args = argsGetter xs (setStart args x2)
argsGetter ("--lines" : x2 : xs) args = argsGetter xs (setLines args x2)
argsGetter ("--window" : x2 : xs) args = argsGetter xs (setWindow args x2)
argsGetter ("--move" : x2 : xs) args = argsGetter xs (setMove args x2)
argsGetter _ _ = Nothing

help :: [String] -> IO ()
help ("-h" : []) = usageWithRet 0
help _ = return ()
