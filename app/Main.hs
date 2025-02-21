import Data.Bits

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

newLeftList :: [State]
newLeftList = cycle [Dead]

newRightList :: [State]
newRightList = [Alive] ++ cycle [Dead]

newWolfram :: Wolfram
newWolfram = (Wolfram newLeftList newRightList)

getBit :: Int -> Int -> Bool
getBit nb index = testBit nb index

main :: IO ()
main = putStrLn (show (getBit 110 7))
