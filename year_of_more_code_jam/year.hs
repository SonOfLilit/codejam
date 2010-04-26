module Main where
import Data.Ratio
import List
import Text.ParserCombinators.Parsec

type IntType = Integer
type Days = IntType
type Tournament = [Days]

data Case = Case Days [Tournament]
data Solution = Solution (Ratio Integer)
instance Show Solution where show=formatSolution

main :: IO ()
main = interact (unlines . zipWith (++) prefixes . map formatSolution . map solve . tryParse parseInput)

prefixes = [ "Case #" ++ show n ++ ": " | n <- [1..]]

solve :: Case -> Solution
solve (Case days [tournament]) = Solution $ (toInteger $ sum (map happinessWhenAt [0..days-1])) % days
    where happinessWhenAt day = length $ filter (roundOccurs day) tournament
          roundOccurs day round = day + round < days
-- TODO: Multiple tournaments
solve (Case _ _) = Solution (0 % 1)

formatSolution (Solution r) = show x ++ "+" ++ show y ++ "/" ++ show z
    where a = numerator r
          b = denominator r
          x = div a b
          y = mod a b
          z = b

number = many1 digit
br = char '\n'

tryParse p input = either (error . show) id (parse p "" input)

parseInput :: Parser [Case]
parseInput = do
    n <- number
    cases <- count (read n) parseCase
    return cases

parseCase :: Parser Case
parseCase = do
    br
    n <- number
    space
    t <- number
    tournaments <- count (read t) parseTournament
    return $ Case (read n) tournaments

parseTournament = do
    br
    m <- number
    rounds <- count (read m - 1) parseRound
    return $ (1 : rounds)

parseRound = do
    space
    d <- number
    return $ (read d) - 1