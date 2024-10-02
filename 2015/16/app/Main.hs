module Main where
import Data.List.Split ( splitOn )
import Data.Map ( Map, fromList, keys, (!), member, lookup )
import Data.Maybe (isNothing, fromJust)
import Data.Foldable (find)

data Sue = Sue { number :: Int, children :: Maybe Int, cats :: Maybe Int,
    samoyeds :: Maybe Int, pomeranians :: Maybe Int, akitas :: Maybe Int,
    vizslas :: Maybe Int, goldfish :: Maybe Int, trees :: Maybe Int, cars :: Maybe Int, perfumes :: Maybe Int } deriving (Show)

trueSue = Sue { number = 0, children = Just 3, cats = Just 7, samoyeds = Just 2, pomeranians = Just 3,
    akitas = Just 0, vizslas = Just 0, goldfish = Just 5, trees = Just 3, cars = Just 2, perfumes = Just 1 }

main = do
    input <- readFile "input.txt"
    let sues = map parseSue (splitOn "\n" input)
    let part1 = [(children, (==)), (cats, (==)), (samoyeds, (==)), (pomeranians, (==)), (akitas, (==)),
            (vizslas, (==)), (goldfish, (==)), (trees, (==)), (cars, (==)), (perfumes, (==))] :: [(Sue -> Maybe Int, Int -> Int -> Bool)]
    let part2 = [(children, (==)), (cats, (>)), (samoyeds, (==)), (pomeranians, (<)), (akitas, (==)),
            (vizslas, (==)), (goldfish, (<)), (trees, (>)), (cars, (==)), (perfumes, (==))] :: [(Sue -> Maybe Int, Int -> Int -> Bool)]
    print (number $ fromJust (find (`validSue` part1) sues), number $ fromJust (find (`validSue` part2) sues))

validSue :: Sue -> [(Sue -> Maybe Int, Int -> Int -> Bool)] -> Bool
validSue sue = all (\(key, comparator) -> isNothing (key sue) || comparator (fromJust (key sue)) (fromJust (key trueSue)))

parseSue :: String -> Sue
parseSue rawSue =
    let (sueData: bel) = splitOn ", " rawSue
        sueNumber = init (splitOn " " sueData !! 1)
        belongings = (splitOn ("Sue " ++ sueNumber ++ ": ") sueData !! 1) : bel
        belongingMap = fromList $ map (\x -> (head (splitOn ": " x), read $ splitOn ": " x !! 1)) belongings
        sue = Sue { number = read sueNumber, children = Nothing, cats = Nothing, samoyeds = Nothing, pomeranians = Nothing,
            akitas = Nothing, vizslas = Nothing, goldfish = Nothing, trees = Nothing, cars = Nothing, perfumes = Nothing }
    in sue { children = Data.Map.lookup "children" belongingMap, cats = Data.Map.lookup "cats" belongingMap,
        samoyeds = Data.Map.lookup "samoyeds" belongingMap, pomeranians = Data.Map.lookup "pomeranians" belongingMap,
        akitas = Data.Map.lookup "akitas" belongingMap, vizslas = Data.Map.lookup "vizslas" belongingMap,
        goldfish = Data.Map.lookup "goldfish" belongingMap, trees = Data.Map.lookup "trees" belongingMap,
        cars = Data.Map.lookup "cars" belongingMap, perfumes = Data.Map.lookup "perfumes" belongingMap }