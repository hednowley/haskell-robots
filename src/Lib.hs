module Lib
    ( someFunc
    ) where

import System.Environment   
import System.IO  
import Data.List
import Data.Text

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
    handle <- openFile fileName ReadMode  
    contents <- hGetContents handle  
    putStr contents  
    hClose handle 

type Dimensions = (Int, Int)

getDimensions :: String -> Maybe Dimensions
getDimensions str =
    case words str of
        n : m -> ReadMaybe n
        w -> Nothing

words   :: String -> [String]
words s =  case dropWhile Char.isSpace s of
                      "" -> []
                      s' -> w : words s''
                            where (w, s'') = break Char.isSpace s'
    
maybeHead :: [a]-> Maybe a
maybeHead list = if length list > 0 then Just (head list) else Nothing