module Main where
import List
import Text.ParserCombinators.Parsec

type Case = (Int, [Int])
type Solution = (Int, Int)

main :: IO ()
main = do
    inputString <- getContents
    let input :: [Case]
        input = tryParse parseInput inputString
        solutions :: [Solution]
        solutions = map solve input
        solutionStrs = zipWith (++) prefixes (map formatSolution solutions)
    putStr $ unlines solutionStrs

solve :: Case -> Solution
solve (credit, items) = let (first : rest) = items
                            creditLeft = credit - first
                            i = findIndex ( (==) creditLeft) rest
                        in case i of
                            Just idx -> (1, idx + 1 + 1) -- 1 because of first, 1 for 0-basedness
                            Nothing -> let (a, b) = solve (credit, rest)
                                       in (a + 1, b + 1)

formatSolution (i, j) = show i ++ " " ++ show j

prefixes = [ "Case #" ++ show n ++ ": " | n <- [1..]]

number = many1 digit
br = char '\n'

tryParse p input = either (error . show) id (parse p "" input)

parseInput :: Parser [Case]
parseInput = do
    n <- number
    br
    cases <- count (read n) parseCase
    return cases

parseCase :: Parser Case
parseCase = do
    c <- number
    br
    i <- number
    br
    items <- count (read i) parseItem
    return (read c, items)

parseItem = do
    p <- number
    space
    return $ read p
