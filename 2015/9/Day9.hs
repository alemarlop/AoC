module Main where
    import Data.List.Split (splitOn)
    import Data.Set (Set, fromList, toList)
    import Data.List (permutations)

    type City = String
    data Distance = Distance City City Int deriving(Show)
    data Route = Route [City] Int deriving(Show)

    main :: IO ()
    main = do
        input <- readFile "input.txt"
        let lines :: [Distance] = map parseLine $ splitOn "\n" input
        let cities :: Set City = getCities lines $ fromList []
        let combinations :: [[City]] = permutations $ toList cities
        let routes :: [Route] = getPossibleRoutes combinations lines
        print $ "Part 1: " ++ show(minimum (map (\(Route _ distance) -> distance) routes))
        print $ "Part 2: " ++ show(maximum $ map (\(Route _ distance) -> distance) routes)

    parseLine :: String -> Distance
    parseLine line = let [city1, "to", city2, "=", distance] = words line
                     in Distance city1 city2 (read distance)

    getCities :: [Distance] -> Set City -> Set City
    getCities [] set = set
    getCities ((Distance city1 city2 _):xs) set = getCities xs $ fromList [city1, city2] `mappend` set

    getPossibleRoutes :: [[City]] -> [Distance] -> [Route]
    getPossibleRoutes [] _ = []
    getPossibleRoutes (combination:xs) distances = Route combination (getDistanceForCombination combination distances) : getPossibleRoutes xs distances

    getDistanceForCombination :: [City] -> [Distance] -> Int
    getDistanceForCombination [] _ = 0
    getDistanceForCombination [city] _ = 0
    getDistanceForCombination (city1:city2:xs) distances = getDistanceBetween city1 city2 distances + getDistanceForCombination (city2:xs) distances

    getDistanceBetween :: City -> City -> [Distance] -> Int
    getDistanceBetween from to [] = 0
    getDistanceBetween from to ((Distance city1 city2 distance):xs) =
        if (from == city1 && to == city2) || (from == city2 && to == city1)
        then distance
        else getDistanceBetween from to xs