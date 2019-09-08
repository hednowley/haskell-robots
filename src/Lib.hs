module Lib
    ( someFunc
    ) where

import System.Environment   
import System.IO  
import Data.List
import Parse
import Walk


someFunc :: IO ()
someFunc = do  
    args <- getArgs
    process args

process :: [String] -> IO ()
process args =
    case (maybeHead args) of
        Nothing -> putStrLn "No path given"
        Just p -> view p

view :: String -> IO ()  
view fileName = do  
    contents <- readFile fileName
    putStr (case (parseConfig contents) of
        Nothing -> "Cannot parse config"
        Just c -> intercalate "/" (run c))



maybeHead :: [a]-> Maybe a
maybeHead [] = Nothing
maybeHead l = Just (head l)