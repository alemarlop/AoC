module Main where

type Code = (Int, Int, Int)

seed = (1, 1, 20151125)

getNextPosition :: Code -> (Int, Int) -> Code
getNextPosition (row, col, x) (targetRow, targetCol)
    | row == targetRow && col == targetCol = (row, col, x)
    | row == 1 = getNextPosition (col + 1, 1, x * 252533 `mod` 33554393) (targetRow, targetCol)
    | otherwise = getNextPosition (row - 1, col + 1, x * 252533 `mod` 33554393) (targetRow, targetCol)


main :: IO ()
main = print $ getNextPosition seed (2978, 3083)
