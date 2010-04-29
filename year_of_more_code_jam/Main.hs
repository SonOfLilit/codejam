module Main where
import Data.Ratio
import Test.HUnit
import Parser
import Year

main :: IO ()
main = interact (unlines . zipWith (++) prefixes . map formatSolution . map solve . tryParse parseInput)

prefixes = [ "Case #" ++ show n ++ ": " | n <- [1..]]
