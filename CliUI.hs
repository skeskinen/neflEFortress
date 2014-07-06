module CliUI where 
import UI
import World
import Terrain

import Control.Applicative hiding ((<|>))
import Control.Lens
import Text.ParserCombinators.Parsec 
import System.Environment
import Control.Monad

newCliUi :: UI ()
newCliUi = return ()

spaces1 :: Parser ()
spaces1 = skipMany1 space

separator :: Parser ()
separator = spaces >> char ',' >> spaces  

parseExpr :: Parser ()
parseExpr = do
    choice genNoTargetCommandParsers
      <|> choice genPointCommandParsers
      <|> choice genAreaCommandParsers
    return ()

for = flip map

type CommandParser = Parser (Command, CommandArgument)

genCommandParsers :: [Command] -> Parser CommandArgument -> [CommandParser] 
genCommandParsers cs argParser = for cs $ \c -> do
    try $ string (c ^. commandName)
    spaces
    arg <- argParser
    return (c, arg)

genNoTargetCommandParsers :: [CommandParser]
genNoTargetCommandParsers = genCommandParsers noTargetCommands parseNoTargetArgument

genPointCommandParsers :: [CommandParser] 
genPointCommandParsers = genCommandParsers pointCommands parsePointArgument

genAreaCommandParsers :: [CommandParser] 
genAreaCommandParsers = genCommandParsers areaCommands parseAreaArgument

parseInt :: Parser Int
parseInt = read <$> many1 digit

parseNoTargetArgument :: Parser CommandArgument
parseNoTargetArgument = return NoTarget

parsePointArgument :: Parser CommandArgument
parsePointArgument = PointArgument <$> parsePoint

parseAreaArgument :: Parser CommandArgument
parseAreaArgument = AreaArgument <$> parseArea

parsePoint :: Parser Point
parsePoint = do
    char '('
    spaces
    x <- parseInt
    separator
    y <- parseInt
    separator
    z <- parseInt
    spaces
    char ')'
    return (x,y,z)

parseArea :: Parser Area
parseArea = do
    p1 <- parsePoint
    spaces
    p2 <- parsePoint
    return (p1, p2)
