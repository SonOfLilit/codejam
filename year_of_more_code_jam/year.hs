module Year where
import Data.Ratio
import List

type Schedule = [Integer]

data Case = Case Integer [Schedule]
instance Show Case where show (Case year schedule) = "Case " ++ show year ++ " " ++ show schedule
data Solution = Solution Rational deriving (Eq)
instance Show Solution where show=formatSolution

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
                  sum $ map (occurence year) (sequence schedules)
              else
                  sum $ map (\s -> meetingOfK year s k) (kplets k schedules)

-- TODO: Real implementation
kplets :: Integer -> [a] -> [[a]]
kplets 1 list = sequence [list]
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
