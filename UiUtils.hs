module UiUtils where

--import Text.ParserCombinators.Parsec 
--import Control.Monad
--import Control.Applicative hiding ((<|>), many)
--import Control.Lens
--import Data.List

import Prelude hiding ((.), id, until)
import Control.Wire
import Control.Wire.Unsafe.Event as X (Event(..), event)

--import Ui
--import Utils

foldWithIndices :: (Double -> Double -> a -> b -> b) -> b -> [[a]] -> b
foldWithIndices = go 0 0
    where go _ _ _ acc [] = acc
          go _ y f acc ([]:ys) = go 0 (y+1) f acc ys
          go x y f acc ((el:xs):ys) = go (x+1) y f (f x y el acc) (xs:ys)

execOnceSet :: Monad m => Wire s e m a b -> Wire s e m a b -> Wire s e m (a, Event c) b
execOnceSet other def = rSwitch def . second (setE $ WGen (\ ds a -> do
    (b, _) <- stepWire other ds a
    return (b, def)))

execOnceMap :: Monad m => (c -> Wire s e m a b) ->
                          Wire s e m a b -> 
                          Wire s e m (a, Event c) b
execOnceMap f def = rSwitch def . second (mapE (\ c -> WGen (\ ds a -> do 
    (b, _) <- stepWire (f c) ds a
    return (b, def))))

mapE :: (a -> b) -> Wire s e m (Event a) (Event b)
mapE f = mkSF_ (event NoEvent (Event . f)) 

setE :: b -> Wire s e m (Event a) (Event b)
setE = accumE const

leftId :: Arrow a => a b c -> a b (b,c)
leftId e = arr id &&& e

whenRight :: Monad m => Either a b -> (b -> m ()) -> m ()
whenRight = flip (either (const (return ())))

{-

commandNames :: [Command] -> [String]
commandNames = sort . map (view commandName)

spaces1 :: Parser ()
spaces1 = skipMany1 space

separator :: Parser ()
separator = spaces >> char ',' >> spaces  

parseCommand :: String -> Either ParseError (Command, CommandArgument)
parseCommand str = parse parseExpr "Input" str

parseCompletion :: String -> [String]
parseCompletion str = filter (not . null) . map (\parser -> 
    let r = parse parser "Comp" str in
      case r of
        Left _ -> ""
        Right a -> a
    ) $ genCompletionParsers (length str)

parseExpr :: CommandParser
parseExpr = 
    choice genNoTargetCommandParsers
      <|> choice genPointCommandParsers
      <|> choice genAreaCommandParsers
      <|> choice genStringCommandParsers

for = flip map

genCompletionParsers :: Int -> [Parser String]
genCompletionParsers n = for allCommands $ \c -> do
    let name = c ^. commandName 
    string $ take n name
    return name 

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

genStringCommandParsers :: [CommandParser] 
genStringCommandParsers = genCommandParsers stringCommands parseStringArgument

parseInt :: Parser Int
parseInt = read <$> many1 digit

parseNoTargetArgument :: Parser CommandArgument
parseNoTargetArgument = return NoTarget

parsePointArgument :: Parser CommandArgument
parsePointArgument = PointArgument <$> parsePoint

parseAreaArgument :: Parser CommandArgument
parseAreaArgument = AreaArgument <$> parseArea

parseStringArgument :: Parser CommandArgument
parseStringArgument = StringArgument <$> many letter

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
-}
