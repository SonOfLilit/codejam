module Main where
import Data.Ratio
import Test.HUnit
import Year

main :: IO ()
main = do
    tests
    return ()

prefixes = [ "Case #" ++ show n ++ ": " | n <- [1..]]

tests = runTestTT $ test [
    sequence [[1, 7, 3], [1, 2]] ~?= [[1, 1], [1, 2], [7, 1], [7, 2], [3, 1], [3, 2]],
    "trivial case" ~: (solve $ Case 1 [[0], [0]]) ~?= Solution (toRational 4),
    occurences 1000 [[0]] ~?= [1000],
    occurences 10 [[0, 20]] ~?= [10],
    occurences 1 [[1, 2, 3, 4, 5]] ~?= [0],
    occurences 1 [[0, 1, 2, 3, 4]] ~?= [1],
    meetingOfK 100 [[0, 3, 7], []] 1 ~?= (meetingOfK 100 [[0, 3, 7]] 1) * 100,
    occurences 100 [[0, 3, 7], [0, 2, 6]] ~?= occurences 100 [[0, 2, 6], [0, 3, 7]],
    (solve $ Case 4 [[0,1,3], [0, 2]]) ~?= Solution (41%8),
    occurences 1 [[0], [0], [0]] ~?= [3, 3, 1],
    (solve $ Case 1 [[0], [0], [0]]) ~?= Solution (9%1),
    test $ [show n ~: meetingOfK 1 (replicate (fromInteger n) [0, 8]) n ~?= 1 | n <- [1..6]],
    test $ [show n ~: meetingOfK 1 (replicate (fromInteger n) [0, 8]) 1 ~?= n | n <- [1..6]],
    test [(solve $ Case 1 (replicate (fromInteger k) [0])) ~?= Solution (k^2%1) | k <- [1..6]],
    -- This fails. It is here just to check performance.
    "long calculation" ~: (solve $ Case 4 (replicate 11 [0, 1, 2])) ~?= Solution (41%8)]
