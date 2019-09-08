module Print
    ( showStates
    ) where

import Walk
import Data.List


showStates :: [State] -> String
showStates states =
    intercalate "\n" (map showState states)
             

showState :: State -> String
showState state = 
    "("
    ++ (show . x $ state)
    ++ ", "
    ++ (show . y $ state)
    ++ ", "
    ++ (showOrientation . orientation $ state)
    ++ ")"
    ++ (showLost . isLost $ state)

showOrientation :: Orientation -> String
showOrientation o =
    case o of
        North -> "N"
        East -> "E"
        South -> "S"
        West -> "W"

showLost :: Bool -> String
showLost lost =
    case lost of
        True -> " LOST"
        False -> ""