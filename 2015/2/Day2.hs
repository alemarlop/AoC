module Main where
import Data.List.Split(splitOn)
import System.Directory.Internal.Prelude (getArgs)
import Data.List (sort)

main :: IO ()
main = do
    (fileName:_) <- getArgs
    fileContent <- readFile fileName
    let lines = splitOn "\n" fileContent
    let parsedLines = map parseLine lines
    print $ "Part 1: " ++ show (part1 parsedLines)
    print $ "Part 2: " ++ show (part2 parsedLines)

parseLine :: String -> [Int]
parseLine line = let elements = splitOn "x" line in
    map read elements

part1 :: [[Int]] -> Int
part1 list = sum $ map getSizeForSingleLine list

part2 :: [[Int]] -> Int
part2 list = sum $ map getRibbonForSingleLine list

getSizeForSingleLine :: [Int] -> Int
getSizeForSingleLine (l:w:h:_) = 
    let areaL = l*w
        areaW = w*h
        areaH = h*l in
    2*areaL + 2*areaW + 2*areaH + minimum [areaL, areaW, areaH]

getRibbonForSingleLine :: [Int] -> Int
getRibbonForSingleLine (l:w:h:_) = (l * w * h) + sum (init $ sort [l,w,h]) * 2