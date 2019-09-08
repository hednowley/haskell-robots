module Walk
    ( 
        Orientation(..)
        , Walk(..)
        , Move(..)
        , Position
        , Dimensions
        , Config(..)
        , run
    ) where

import Data.List

data Orientation = North | East | South | West
data Move = MoveForward | RotateLeft | RotateRight


type Dimensions = (Integer, Integer)
type Position = (Integer, Integer, Orientation)

data Walk = Walk {
    initial :: Position
    , moves :: [Move]
}
        
data Config =  Config {
    dims :: Dimensions
    , walks :: [Walk]
}


run :: Config -> [String]
run c = map (runSingle (dims c)) (walks c)


runSingle  :: Dimensions -> Walk -> String
runSingle dims walk =
    let (x, y, o) = getFinalPosition dims walk
    in show x ++ "/" ++ show y ++ showOrientation o ++ "//"


getFinalPosition :: Dimensions -> Walk -> Position
getFinalPosition dims walk =
    foldl (\pos move -> applyMove dims move pos) (initial walk) (moves walk)

applyMove :: Dimensions -> Move -> Position -> Position
applyMove dims move (x, y, o) =
    case move of
        MoveForward -> let new = moveForward (x, y, o) in if (isWithinBounds dims new) then new else (x, y, o)
        RotateLeft -> (x, y, rotateLeft o)
        RotateRight -> (x, y, rotateRight o)


rotateLeft :: Orientation -> Orientation
rotateLeft o = 
    case o of
        North -> West
        West -> South
        South -> East
        East -> North

rotateRight :: Orientation -> Orientation
rotateRight o = 
    case o of
        North -> East
        East -> South
        South -> West
        West -> North

moveForward :: Position -> Position
moveForward (x, y, o) =
    case o of
        North -> (x, y + 1, o)
        East -> (x + 1, y, o)
        South -> (x, y - 1, o)
        West -> (x - 1, y, o)

showOrientation :: Orientation -> String
showOrientation o =
    case o of
        North -> "N"
        East -> "E"
        South -> "S"
        West -> "W"

isWithinBounds :: Dimensions -> Position -> Bool
isWithinBounds (m, n) (x, y, _) = x > -1 && x < m && y > -1 && y < n