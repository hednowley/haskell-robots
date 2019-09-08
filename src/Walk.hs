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
    let (x, y) = getFinalPosition dims walk
    in show(x) ++ "/" ++ show(y) ++ "//"


getFinalPosition :: Dimensions -> Walk -> Position
getFinalPosition dims walk =
    let apply = applyMove dims
    in foldr (\pos move -> apply move pos) (initial walk) (moves walk)

applyMove :: Dimensions -> Move -> Position -> Position
applyMove dims move (x, y, o) =
    case move of
        MoveForward -> moveForward (x, y, o)
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
        North -> (y + 1, x, o)
        East -> (y, x + 1, o)
        South -> (y - 1, x, o)
        West -> (y, x - 1, o)