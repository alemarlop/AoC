module Main where
import Data.List.Split (splitOn)

data Point = Point (Int, Int) Char deriving Show
type Matrix = [String]

main :: IO ()
main = do
    input <- readFile "input.txt"
    let ps = toPoints $ splitOn "\n" input
    print (length $ filter (\(Point _ st) -> st == '#') (iterate (runRound (const False)) ps !! 100),
        length $ filter (\(Point _ st) -> st == '#') (iterate (runRound isCorner) (turnCornersOn ps) !! 100))

toPoints :: Matrix -> [Point]
toPoints m = [Point (row, col) (m !! row !! col) | row <- [0..length m - 1], col <- [0..length (head m) - 1]]

isCorner :: Point -> Bool
isCorner (Point (row, col) _) = (row == 0 || row == 99) && (col == 0 || col == 99)

turnCornersOn :: [Point] -> [Point]
turnCornersOn = map (\(Point (row, col) st) -> if isCorner (Point (row, col) st) then Point (row, col) '#' else Point (row, col) st)

runRound :: (Point -> Bool) -> [Point] -> [Point]
runRound f ps = map (nextPoint ps f) ps

nextPoint :: [Point]-> (Point -> Bool) -> Point -> Point
nextPoint allPoints f (Point (row, col) st) =
    let enlighten = length $ filter (\(Point (row', col') st') -> abs (row - row') <= 1 && abs (col - col') <= 1 && st' == '#') allPoints
    in case st of
        '#' -> Point (row, col) (if enlighten == 3 || enlighten == 4 || f (Point (row, col) st) then '#' else '.')
        _ -> Point (row, col) (if enlighten == 3 then '#' else '.')