module Lib
    ( start
    ) where

import System.Environment   
import System.IO  
import Data.List
import Parse
import Walk
import Print

-- Entry point
start :: IO ()
start = do  
    args <- getArgs
    case maybeHead args of
        Nothing -> putStrLn "No path given"
        Just p -> processFile p

processFile :: String -> IO ()  
processFile fileName = do  
    contents <- readFile fileName
    putStr . getResult . parseConfig $ contents

getResult :: Maybe Config -> String
getResult maybe =
    case maybe of
        Nothing -> "Cannot parse config"
        Just config -> showStates . run $ config


maybeHead :: [a]-> Maybe a
maybeHead [] = Nothing
maybeHead list = Just . head $ list