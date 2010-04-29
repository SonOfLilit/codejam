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
                  sum $ map (\s -> meetingOfK year s k) (combine schedules k)

combine _ r | r < 0	= error "Zero or more elements should be extracted."
combine _ 0	= [[]]
combine [] _ = []
combine (x:xs) r = map (x:) (combine xs (r - 1)) ++ combine xs r

ncr _ 0	= 1
ncr 0 _ = 0
ncr n r = ncr (n-1) (r-1) + ncr (n-1) r

occurence year [o] = max 0 (year - o)
occurence year offsets = max 0 (year - maximum offsets)

correctOccurences :: [Integer] -> [Integer]
correctOccurences list = correct list 1

correct :: [Integer] -> Integer -> [Integer]
correct last@[_] _ = last
correct (this : rest) k = (thisCorrected : restCorrected)
    where thisCorrected = this - sum (zipWith (*) (map (`ncr` k) [k+1..]) restCorrected)
          restCorrected = correct rest (k+1)

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
