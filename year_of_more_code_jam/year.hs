module Main where
import Data.Ratio
import List
import Text.ParserCombinators.Parsec
import Test.HUnit

type Schedule = [Integer]

data Case = Case Integer [Schedule]
instance Show Case where show (Case year schedule) = "Case " ++ show year ++ " " ++ show schedule
data Solution = Solution Rational deriving (Eq)
instance Show Solution where show=formatSolution

main :: IO ()
main = interact (unlines . zipWith (++) prefixes . map formatSolution . map solve . tryParse parseInput)

prefixes = [ "Case #" ++ show n ++ ": " | n <- [1..]]

tryParse p input = either (error . show) id (parse p "" input)

tests = runTestTT $ test [
    cartesian [1, 7, 3] [1, 2] ~?= [(1, 1), (1, 2), (7, 1), (7, 2), (3, 1), (3, 2)],
    (solve $ Case 1 [[0], [0]]) ~?= Solution (toRational 4)]

solve :: Case -> Solution
solve (Case year schedules) = Solution $ occurencesToHapiness year (occurences year schedules)

occurences :: Integer -> [Schedule] -> [Integer]
occurences year [schedule] = [occurencesOne year schedule]
occurences year schedules@[s1, s2] = [singleOcc, twoOcc]
    where singleOcc = (sum $ map (occurencesOne year) schedules) * year
          twoOcc = (sum $ map (occurTogether year) (cartesian s1 s2))
occurences year schedules = [-1]

cartesian a b = [(x, y) | x<-a, y<-b]

occurencesOne year schedule = sum $ map (occurence year) schedule

occurence year offset = max 0 (year - offset)

occurTogether year (o1, o2) = occurence year combinedOffset
    where combinedOffset = max o1 o2

occurencesToHapiness :: Integer -> [Integer] -> Rational
occurencesToHapiness year [one] = one % year
occurencesToHapiness year [a, b] = ((one - 2*two)*(1^2) + two*(2^2)) / y^2
    where one = toRational a
          two = toRational b
          y = toRational year

formatSolution (Solution r) = show x ++ "+" ++ show y ++ "/" ++ show z
    where a = numerator r
          b = denominator r
          x = div a b
          y = mod a b
          z = b

number = many1 digit
br = char '\n'

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
