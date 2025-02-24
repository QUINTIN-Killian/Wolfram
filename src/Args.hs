{-
-- EPITECH PROJECT, 2025
-- Wolfram
-- File description:
-- Setters
-}

module Args (exploreArgs, setArgs, help) where

import ArgsData
import Utils

setRule :: Args -> String -> Args
setRule (Args r s l w m err) [] = (Args r s l w m True)
setRule (Args r s l w m err) str =
    if not (isInt str && isIntInRange (read str) 0 255)
    then (Args r s l w m True)
    else (Args (Just (read str)) s l w m err)

setStart :: Args -> String -> Args
setStart (Args r s l w m err) [] = (Args r s l w m True)
setStart (Args r s l w m err) str =
    if not (isInt str && isPos (read str))
    then (Args r s l w m True)
    else (Args r (Just (read str)) l w m err)

setLines :: Args -> String -> Args
setLines (Args r s l w m err) [] = (Args r s l w m True)
setLines (Args r s l w m err) str =
    if not (isInt str && isPos (read str))
    then (Args r s l w m True)
    else (Args r s (Just (read str)) w m err)

setWindow :: Args -> String -> Args
setWindow (Args r s l w m err) [] = (Args r s l w m True)
setWindow (Args r s l w m err) str =
    if not (isInt str && isPos (read str))
    then (Args r s l w m True)
    else (Args r s l (Just (read str)) m err)

setMove :: Args -> String -> Args
setMove (Args r s l w m err) [] = (Args r s l w m True)
setMove (Args r s l w m err) str =
    if not (isInt str)
    then (Args r s l w m True)
    else (Args r s l w (Just (read str)) err)

exploreArgs :: [String] -> Args -> Args
exploreArgs _ (Args r s l w m True) = (Args r s l w m True)
exploreArgs [] (Args r s l w m err) = (Args r s l w m err)
exploreArgs (x:[]) (Args r s l w m err) = (Args r s l w m True)
exploreArgs (x:xs) args@(Args r s l w m err) = case x of
    "--rule" -> exploreArgs (tail xs) (setRule args (head xs))
    "--start" -> exploreArgs (tail xs) (setStart args (head xs))
    "--lines" -> exploreArgs (tail xs) (setLines args (head xs))
    "--window" -> exploreArgs (tail xs) (setWindow args (head xs))
    "--move" -> exploreArgs (tail xs) (setMove args (head xs))
    _ -> (Args r s l w m True)

setArgs :: Args -> Args
setArgs (Args r Nothing l w m err) = setArgs (Args r (Just 0) l w m err)
setArgs (Args r s Nothing w m err) = setArgs (Args r s (Just (-1)) w m err)
setArgs (Args r s l Nothing m err) = setArgs (Args r s l (Just 80) m err)
setArgs (Args r s l w Nothing err) = setArgs (Args r s l w (Just 0) err)
setArgs (Args r s l w m err) = (Args r s l w m err)

help :: [String] -> IO ()
help ("-h":[]) = usageWithRet 0
help _ = return ()
