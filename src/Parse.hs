module Parse
    ( parseConfig
    ) where

import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)
import Walk

-- Try to parse the given string as a config file
parseConfig :: String -> Maybe Config
parseConfig contents = 
    case (parse robotsFile "" contents) of
        Prelude.Left _ -> Nothing
        Prelude.Right c -> Just c

robotsFile :: Parser Config
robotsFile = do
    dims <- dimensions
    endOfLine
    walks <- walk `sepBy` endOfLine
    return (Config dims walks)

dimensions :: Parser Dimensions
dimensions = 
    do
        n <- num
        spaces
        m <- num 
        return (n, m)

state :: Parser Walk.State
state = 
    do
        char '('
        x <- num
        char ','
        spaces
        y <- num 
        char ','
        spaces
        o <- Parse.orientation
        char ')'
        return (Walk.State x y o False)

orientation :: Parser Orientation
orientation = 
    North <$ char 'N'
    <|> East <$ char 'E'
    <|> South <$ char 'S'
    <|> West <$ char 'W'

move :: Parser Move
move = 
    MoveForward <$ char 'F'
    <|> RotateLeft <$ char 'L'
    <|> RotateRight <$ char 'R'


walk :: Parser Walk
walk =
    do
        initial <- state
        spaces
        moves <- many move
        return (Walk initial moves)

-- Parse an integer
num :: Parser Integer
num = do
    n <- many1 digit
    return (read n)