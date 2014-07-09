module CliUI where 

import Control.Applicative hiding ((<|>))
import Control.Lens
import Text.ParserCombinators.Parsec 
import System.Environment
import Control.Monad
import Control.Monad.State
import System.IO

import Utils
import UI
import World
import Terrain

cliClear :: UI ()
cliClear = liftIO $ putStr "\ESC[2J"

cliDraw :: UI ()
cliDraw = do
    cliClear
    t <- use (uiWorld . worldTerrain)
    f <- use (uiCamera . _3)
    let floor = getFloor t f
    liftIO $ print floor
    liftIO $ putStr "> "
    liftIO $ hFlush stdout

cliEval :: UI ()
cliEval = do
    str <- liftIO $ getLine
    let r = parse parseExpr "Input" str
    case r of
         Left err -> return ()
         Right (com, arg) -> (com ^. commandFunction) arg

until_ :: Monad m => m Bool -> m () -> m ()
until_ pred action = do
    action
    c <- pred
    if c then until_ pred action else return ()

newCliUi :: UI ()
newCliUi = until_ (not <$> use uiQuit) (cliDraw >> cliEval)

spaces1 :: Parser ()
spaces1 = skipMany1 space

separator :: Parser ()
separator = spaces >> char ',' >> spaces  

parseExpr :: CommandParser
parseExpr = do
    choice genNoTargetCommandParsers
      <|> choice genPointCommandParsers
      <|> choice genAreaCommandParsers

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
