module Parser.Duration (parseDuration, Seconds) where

import Relude hiding (some)

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char (char, digitChar, numberChar, space, string)

type Parser = Parsec Void String
type Seconds = Int

data Unit = UnitDays | UnitHours | UnitMinutes deriving (Show, Eq)

maybeS :: Parser String
maybeS = option "" (string "s")

unitParser :: Parser Unit
unitParser =
    choice
        [ UnitDays <$ (string "day" <> maybeS)
        , UnitHours <$ (string "hour" <> maybeS)
        , UnitMinutes <$ (string "minute" <> maybeS)
        ]

intParser :: Parser Int
intParser = (fromMaybe 0 . readMaybe) <$> some digitChar

parseSimpleDuration :: Parser Seconds
parseSimpleDuration = do
    v <- intParser
    space
    unit <- unitParser
    pure $ case unit of
        UnitMinutes -> v * 60
        UnitHours -> v * 3600
        UnitDays -> v * 24 * 3600

parseDuration :: String -> Either (ParseErrorBundle String Void) Seconds
parseDuration = parse parseSimpleDuration ""