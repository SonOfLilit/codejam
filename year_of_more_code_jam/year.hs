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
    cartesian [[1, 7, 3], [1, 2]] ~?= [[1, 1], [1, 2], [7, 1], [7, 2], [3, 1], [3, 2]],
    "trivial case" ~: (solve $ Case 1 [[0], [0]]) ~?= Solution (toRational 4),
    occurences 1000 [[0]] ~?= [1000],
    occurences 10 [[0, 20]] ~?= [10],
    occurences 1 [[1, 2, 3, 4, 5]] ~?= [0],
    occurences 1 [[0, 1, 2, 3, 4]] ~?= [1],
    meetingOfK 100 [[0, 3, 7], []] 1 ~?= (meetingOfK 100 [[0, 3, 7]] 1) * 100,
    occurences 100 [[0, 3, 7], [0, 2, 6]] ~?= occurences 100 [[0, 2, 6], [0, 3, 7]],
    (solve $ Case 4 [[0,1,3], [0, 2]]) ~?= Solution (41%8),
    occurences 1 [[0], [0], [0]] ~?= [3, 3, 1],
    (solve $ Case 1 [[0], [0], [0]]) ~?= Solution (9%1)]

solve :: Case -> Solution
solve (Case year schedules) = Solution $ occurencesToHapiness year $ correctOccurences (occurences year schedules)

occurences :: Integer -> [Schedule] -> [Integer]
occurences year schedules = map (meetingOfK year schedules) [1..toInteger $ length schedules]

meetingOfK :: Integer -> [Schedule] -> Integer -> Integer
meetingOfK year schedules k = spaceFactor * occurencesInLenSchedDimensions
    where spaceFactor = year^restOfDimensions
          restOfDimensions = lenSched - k
          lenSched = toInteger (length schedules)
          occurencesInLenSchedDimensions =
              if k == lenSched then
                  sum $ map (occurence year) (cartesian schedules)
              else
                  sum $ map (\s -> meetingOfK year s k) (kplets k schedules)

cartesian :: [[a]] -> [[a]]
cartesian [a] = [[x] | x <- a]
cartesian [a, b] = [[x, y] | x<-a, y<-b]
cartesian [a, b, c] = [[x, y, z] | x<-a, y<-b, z<-c]

kplets :: Integer -> [a] -> [[a]]
kplets 1 list = cartesian [list]
kplets 2 [a, b, c] = [[a, b], [a, c], [b, c]]

occurence year offsets = max 0 (year - maximum offsets)

correctOccurences :: [Integer] -> [Integer]
correctOccurences list = correct list 1

correct :: [Integer] -> Integer -> [Integer]
correct last@[_] _ = last
correct (this : rest) k = (this - sum (zipWith (*) ks corrected) : corrected)
    where ks = [k+1..]
          corrected :: [Integer]
          corrected = correct rest (k+1)

occurencesToHapiness :: Integer -> [Integer] -> Rational
occurencesToHapiness year list = (sum scores) % (year^listLen)
    where scores = zipWith (*) list squares
          squares = (map (^2) [1..])
          listLen = toInteger $ length list

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
