module Main where
import System.Directory.Internal.Prelude (getArgs)

main :: IO ()
main = do
    (fileName: _) <- getArgs
    fileContent <- readFile fileName
    print ("Part 1: " ++ show (day1Part1 fileContent))
    print ("Part 2: " ++ show (day1Part2 fileContent))

day1Part1 :: [Char] -> Int
day1Part1 fileContent = solveDay1Part1 fileContent 0

solveDay1Part1 :: [Char] -> Int -> Int
solveDay1Part1 [] currentNumber = currentNumber
solveDay1Part1 ('(':xs) currentNumer = solveDay1Part1 xs (currentNumer + 1)
solveDay1Part1 (')':xs) currentNumer = solveDay1Part1 xs (currentNumer - 1)

day1Part2 :: [Char] -> Int
day1Part2 fileContent = solveDay1Part2 fileContent 0 1

solveDay1Part2 :: [Char] -> Int -> Int -> Int
solveDay1Part2 _ (-1) currentIndex = currentIndex - 1
solveDay1Part2 [] currentNumber currentIndex = -1
solveDay1Part2 ('(':xs) currentNumer currentIndex = solveDay1Part2 xs (currentNumer + 1) (currentIndex + 1)
solveDay1Part2 (')':xs) currentNumer currentIndex = solveDay1Part2 xs (currentNumer - 1) (currentIndex + 1)