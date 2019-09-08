module Parse
    ( parseConfig
    ) where

import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)
import Walk
                
num :: Parser Integer
num = do
    n <- many1 digit
    return (read n)
    
parseConfig :: String -> Maybe Config
parseConfig contents = 
    case (parse robotsFile "(unknown)" contents) of
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

position :: Parser Position
position = 
    do
        char '('
        n <- num
        char ','
        spaces
        m <- num 
        char ','
        spaces
        o <- orientation
        char ')'
        return (n, m, o)

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
        initial <- position
        spaces
        moves <- many move
        return (Walk initial moves)