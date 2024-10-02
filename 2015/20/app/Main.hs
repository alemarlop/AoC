module Main where

divisors :: Int -> [Int]
divisors n = [x | x <- [1..n], n `mod` x == 0]

main :: IO ()
main = print (getHouseNumber 34000000 0 10 34000000, getHouseNumber 34000000 0 11 50)

getHouseNumber :: Int -> Int -> Int -> Int -> Int
getHouseNumber target current times limit =
    let freeDivs = [x * times | x <- divisors current, current `div` x <= limit]
    in if sum freeDivs >= target then current else getHouseNumber target (current + (2 * 3 * 4 * 5 * 6 * 7)) times limit
