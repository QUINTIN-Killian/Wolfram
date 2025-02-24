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
setRule (Args rule start nbLines window move err) [] =
    (Args rule start nbLines window move True)
setRule (Args rule start nbLines window move err) str =
    if not (isInt str && isIntInRange (read str) 0 255)
    then (Args rule start nbLines window move True)
    else (Args (Just (read str)) start nbLines window move err)

setStart :: Args -> String -> Args
setStart (Args rule start nbLines window move err) [] =
    (Args rule start nbLines window move True)
setStart (Args rule start nbLines window move err) str =
    if not (isInt str && isPos (read str))
    then (Args rule start nbLines window move True)
    else (Args rule (Just (read str)) nbLines window move err)

setLines :: Args -> String -> Args
setLines (Args rule start nbLines window move err) [] =
    (Args rule start nbLines window move True)
setLines (Args rule start nbLines window move err) str =
    if not (isInt str && isPos (read str))
    then (Args rule start nbLines window move True)
    else (Args rule start (Just (read str)) window move err)

setWindow :: Args -> String -> Args
setWindow (Args rule start nbLines window move err) [] =
    (Args rule start nbLines window move True)
setWindow (Args rule start nbLines window move err) str =
    if not (isInt str && isPos (read str))
    then (Args rule start nbLines window move True)
    else (Args rule start nbLines (Just (read str)) move err)

setMove :: Args -> String -> Args
setMove (Args rule start nbLines window move err) [] =
    (Args rule start nbLines window move True)
setMove (Args rule start nbLines window move err) str =
    if not (isInt str)
    then (Args rule start nbLines window move True)
    else (Args rule start nbLines window (Just (read str)) err)

exploreArgs :: [String] -> Args -> Args
exploreArgs _ (Args rule start nbLines window move True) =
    (Args rule start nbLines window move True)
exploreArgs [] (Args rule start nbLines window move err) =
    (Args rule start nbLines window move err)
exploreArgs (x:[]) (Args rule start nbLines window move err) =
    (Args rule start nbLines window move True)
exploreArgs (x:xs) args@(Args rule start nbLines window move err) = case x of
    "--rule" -> exploreArgs (tail xs) (setRule args (head xs))
    "--start" -> exploreArgs (tail xs) (setStart args (head xs))
    "--lines" -> exploreArgs (tail xs) (setLines args (head xs))
    "--window" -> exploreArgs (tail xs) (setWindow args (head xs))
    "--move" -> exploreArgs (tail xs) (setMove args (head xs))
    _ -> (Args rule start nbLines window move True)

setArgs :: Args -> Args
setArgs (Args rule Nothing nbLines window move err) =
    setArgs (Args rule (Just 0) nbLines window move err)
setArgs (Args rule start Nothing window move err) =
    setArgs (Args rule start (Just (-1)) window move err)
setArgs (Args rule start nbLines Nothing move err) =
    setArgs (Args rule start nbLines (Just 80) move err)
setArgs (Args rule start nbLines window Nothing err) =
    setArgs (Args rule start nbLines window (Just 0) err)
setArgs (Args rule start nbLines window move err) =
    (Args rule start nbLines window move err)

help :: [String] -> IO ()
help ("-h":[]) = usageWithRet 0
help _ = return ()
