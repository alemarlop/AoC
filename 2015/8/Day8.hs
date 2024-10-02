module Main where
    import Data.List.Split (splitOn)
    import Data.Char (isDigit)

    main :: IO ()
    main = do
        input <- readFile "input.txt"
        let words :: [String] = splitOn "\n" input
        let totalLength = sum $ map length words
        print $ "Part 1: " ++ show (totalLength - sum (map calculateValue words))
        print $ "Part 2: " ++ show (sum (map expand words) - totalLength)

    calculateValue :: String -> Int
    calculateValue = (-2+) . length . removeHexaLaunch . removeSlashesLaunch . removeQuotesLaunch

    expand :: String -> Int
    expand [] = 2
    expand (x:xs) = case x of
        '"' -> 2 + expand xs
        '\\' -> 2 + expand xs
        _ -> 1 + expand xs

    removeQuotesLaunch :: String -> String
    removeQuotesLaunch p = removeQuotes p []
    removeQuotes :: String -> String -> String
    removeQuotes [] res = res
    removeQuotes ('"':xs) acc = removeQuotes xs (acc ++ ['*'])
    removeQuotes (x:y:xs) acc = case (x, y) of
        ('\\', '"') -> removeQuotes xs (acc ++ ['*'])
        _ -> removeQuotes (y:xs) (acc ++ [x])
    removeQuotes (x:xs) acc = removeQuotes xs (acc ++ [x])

    removeSlashesLaunch :: String -> String
    removeSlashesLaunch p = removeSlashes p []
    removeSlashes :: String -> String -> String
    removeSlashes [] res = res
    removeSlashes ('\\':'\\':xs) acc = removeSlashes xs (acc ++ ['*'])
    removeSlashes (x:xs) acc = removeSlashes xs (acc ++ [x])

    removeHexaLaunch :: String -> String
    removeHexaLaunch p = removeHexadecimal p []
    removeHexadecimal :: String -> String -> String
    removeHexadecimal [] res = res
    removeHexadecimal ('\\':'x':y:z:xs) acc = removeHexadecimal xs acc ++ ['*']
    removeHexadecimal (x:xs) acc = removeHexadecimal xs acc ++ [x]