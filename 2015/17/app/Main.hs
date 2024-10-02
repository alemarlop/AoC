module Main where
import Data.List (subsequences)

main :: IO ()
main = let partialResult = [x | x <- subsequences [50,44,11,49,42,46,18,32,26,40,21,7,18,43,10,47,36,24,22,40], sum x == 150]
    in print (length partialResult, length $ filter ((==) (minimum $ map length partialResult) . length) partialResult)
