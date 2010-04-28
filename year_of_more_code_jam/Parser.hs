module Parser where
import Data.Ratio
import Text.ParserCombinators.Parsec
import Year

tryParse p input = either (error . show) id (parse p "" input)

number = many1 digit
br = char '\n'

parseInput :: Parser [Case]
parseInput = do
    n <- number
    cases <- count (read n) parseCase
    eof
    return cases

parseCase :: Parser Case
parseCase = do
    br
    n <- number
    space
    t <- number
    tournaments <- count (read t) parseTournament
    return $ Case (read n) tournaments

parseTournament :: Parser Schedule
parseTournament = do
    br
    m <- number
    rounds <- count (read m - 1) parseRound
    return $ (0 : rounds)

parseRound :: Parser Integer
parseRound = do
    space
    d <- number
    return $ read d - 1
