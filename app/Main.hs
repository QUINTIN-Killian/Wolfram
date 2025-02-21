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

newLeftRow :: [State]
newLeftRow = repeat Dead

newRightRow :: [State]
newRightRow = Alive : repeat Dead

newWolfram :: Wolfram
newWolfram = (Wolfram newLeftRow newRightRow)

getBit :: Int -> Int -> Bool
getBit nb index = testBit nb index

castState :: Bool -> State
castState True = Alive
castState _ = Dead

getState :: Int -> (State, State, State) -> State
getState ruleNb (Dead, Dead, Dead) = castState (getBit ruleNb 0)
getState ruleNb (Dead, Dead, Alive) = castState (getBit ruleNb 1)
getState ruleNb (Dead, Alive, Dead) = castState (getBit ruleNb 2)
getState ruleNb (Dead, Alive, Alive) = castState (getBit ruleNb 3)
getState ruleNb (Alive, Dead, Dead) = castState (getBit ruleNb 4)
getState ruleNb (Alive, Dead, Alive) = castState (getBit ruleNb 5)
getState ruleNb (Alive, Alive, Dead) = castState (getBit ruleNb 6)
getState ruleNb (Alive, Alive, Alive) = castState (getBit ruleNb 7)

getRow :: Int -> [State] -> [State]
getRow ruleNb (x1:x2:x3:xs) = (getState ruleNb (x1, x2, x3)) : (getRow ruleNb (x2:x3:xs))
getRow _ _ = []

getCenter :: Int -> Wolfram -> State
getCenter ruleNb (Wolfram (x:xs) (y1:y2:ys)) = getState ruleNb (x, y1, y2)

main :: IO ()
main = do
    putStrLn (showStateList (reverse (take 5 (getRow 110 left))))
    putStrLn (show (getCenter 110 (Wolfram left right)))
    putStrLn (showStateList (take 5 (getRow 110 right)))
    where (Wolfram left right) = newWolfram
