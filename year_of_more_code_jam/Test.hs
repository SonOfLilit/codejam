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
    -- This fails. It is here just to check performance.
    (solve $ Case 4 [[0, 1], [0, 1], [0, 1], [0, 1], [0, 1], [0, 1], [0, 1], [0, 1], [0, 1]]) ~?= Solution (41%8)]
