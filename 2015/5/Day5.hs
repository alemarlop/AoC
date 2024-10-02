module Main where
import System.Directory.Internal.Prelude (getArgs)
import Data.List.Split (splitOn)
import Data.List (isInfixOf)

vowels :: [Char]
vowels = ['a', 'e', 'i', 'o', 'u']

forbiddenWords :: [String]
forbiddenWords = ["ab", "cd", "pq", "xy"]

main :: IO()
main = do
    (fileName:_) <- getArgs
    fileContent <- readFile fileName
    let lines = splitOn "\n" fileContent
    print $ "Part 1: "++ show (countNiceStrings lines 0 isNice)
    print $ "Part 2: "++ show (countNiceStrings lines 0 isNicePart2)

countNiceStrings :: [String] -> Int -> (String -> Bool) ->  Int
countNiceStrings xs count func = foldl (\ count x -> if func x then count + 1 else count) count xs

isNice :: String -> Bool
isNice word = containsThreeVowels word && containsDuplicatedChar word && not (containsForbiddenWord forbiddenWords word)

isNicePart2 :: String -> Bool
isNicePart2 word = pairAppearsTwice word && repeatedLetter word

containsThreeVowels :: String -> Bool
containsThreeVowels word = containsThreeVowels' word 0

containsThreeVowels' :: String -> Int -> Bool
containsThreeVowels' _ 3 = True
containsThreeVowels' [] _ = False
containsThreeVowels' (x:xs) count = containsThreeVowels' xs (if x `elem` vowels then count + 1 else count)

containsDuplicatedChar :: String -> Bool
containsDuplicatedChar word = containsDuplicatedChar' word word

containsDuplicatedChar' :: String -> String -> Bool
containsDuplicatedChar' [] _ = False
containsDuplicatedChar' (x:xs) word = ([x,x] `isInfixOf` word) || containsDuplicatedChar' xs word

containsForbiddenWord :: [String] -> String -> Bool
containsForbiddenWord [] _ = False
containsForbiddenWord (x:xs) word = (x `isInfixOf` word) || containsForbiddenWord xs word

pairAppearsTwice :: String -> Bool
pairAppearsTwice (x:y:z) = ([x,y] `isInfixOf` z) || pairAppearsTwice (y:z)
pairAppearsTwice _ = False

repeatedLetter :: String -> Bool
repeatedLetter (x:y:x':xs) = (x == x') || repeatedLetter (y:x':xs)
repeatedLetter _ = False