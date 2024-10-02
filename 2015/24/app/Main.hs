module Main where
import Data.List (subsequences)

list = [1,3,5,11,13,17,19,23,29,31,41,43,47,53,59,61,67,71,73,79,83,89,97,101,103,107,109,113]

main :: IO ()
main = print $ map (calculateQuantumEntanglement list) [3,4]

calculateQuantumEntanglement :: [Int] -> Int -> Int
calculateQuantumEntanglement list groups = let
    target = sum list `div` groups
    seqs = [seq | seq <- subsequences list, sum seq == target]
    len = minimum $ map length seqs in
    minimum [product seq | seq <- seqs, length seq == len]


