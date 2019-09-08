module Walk
    ( 
        Orientation(..)
        , Walk(..)
        , Move(..)
        , State(..)
        , Dimensions
        , Config(..)
        , run
    ) where

import Data.List

data Orientation = North | East | South | West
data Move = MoveForward | RotateLeft | RotateRight


type Dimensions = (Integer, Integer)

data State = State {
    x :: Integer,
    y :: Integer,
    orientation :: Orientation,
    isLost :: Bool
}

data Walk = Walk {
    initial :: State
    , moves :: [Move]
}
        
data Config =  Config {
    dims :: Dimensions
    , walks :: [Walk]
}

-- Get the final state of every walk
run :: Config -> [State]
run c = map (getFinalPosition . dims $ c) (walks c)

-- Apply all the walk's command and return its final state
getFinalPosition :: Dimensions -> Walk -> State
getFinalPosition dims walk =
    foldl (\state move -> applyMove dims move state) (initial walk) (moves walk)

applyMove :: Dimensions -> Move -> State -> State
applyMove dims move state =
    if isLost state then state else
    case move of
        MoveForward -> 
            let moved = moveForward state
            in if isWithinBounds dims moved then moved else state { isLost = True}
        RotateLeft -> state { orientation = rotateLeft (orientation state) }
        RotateRight -> state { orientation = rotateRight (orientation state) }


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

moveForward :: State -> State
moveForward state =
    case orientation state of
        North -> state { y = y state + 1}
        East -> state { x = x state + 1}
        South -> state { y = y state - 1}
        West -> state { x = x state - 1}


-- Whether the given state lies within the box dimensions
isWithinBounds :: Dimensions -> State -> Bool
isWithinBounds (m, n) State { x=x, y=y } = x > -1 && x < m && y > -1 && y < n