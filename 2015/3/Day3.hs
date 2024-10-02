module Main where
import System.Directory.Internal.Prelude (getArgs)

data Turn = Santa | Robot

main :: IO ()
main = do
    (fileName:_) <- getArgs
    fileContent <- readFile fileName
    print $ "Part 1: " ++ show (part1 fileContent (0,0) [(0,0)])
    print $ "Part 2: " ++ show (part2 Santa fileContent (0,0) (0,0) [(0,0)])

part1 :: [Char] -> (Int, Int) -> [(Int, Int)] -> Int
part1 [] _ visited = length visited
part1 (x:xs) currentPosition visited = let newPosition = getNewPosition x currentPosition in 
    part1 xs newPosition (addIfNotVisited visited newPosition)

part2 :: Turn -> [Char] -> (Int, Int) -> (Int, Int) -> [(Int, Int)] -> Int
part2 _ [] _ _ visited = length visited
part2 Santa (x:xs) santaPosition robotPosition visited = let newPosition = getNewPosition x santaPosition in
    part2 Robot xs newPosition robotPosition (addIfNotVisited visited newPosition)
part2 Robot (x:xs) santaPosition robotPosition visited = let newPosition = getNewPosition x robotPosition in
    part2 Santa xs santaPosition newPosition (addIfNotVisited visited newPosition)

getNewPosition :: Char -> (Int, Int) -> (Int, Int)
getNewPosition '^' currentPosition = (fst currentPosition, snd currentPosition - 1)
getNewPosition 'v' currentPosition = (fst currentPosition, snd currentPosition + 1)
getNewPosition '<' currentPosition = (fst currentPosition - 1, snd currentPosition)
getNewPosition '>' currentPosition = (fst currentPosition + 1, snd currentPosition)


addIfNotVisited :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
addIfNotVisited visited current = if current `elem` visited then visited else current:visited