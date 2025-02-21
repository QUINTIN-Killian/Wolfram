module Data (State) where

data State = Dead | Alive deriving Eq

instance Show State where
    show Dead = " "
    show Alive = "*"
