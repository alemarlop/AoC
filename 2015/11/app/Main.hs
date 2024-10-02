module Main where
    import Data.Map (fromList, (!), size)
    import BasePrelude (group)

    charToInt = Data.Map.fromList $ zip ['a'..'z'] [1..26]
    intToChart = Data.Map.fromList $ zip [1..26] ['a'..'z']

    encode = map (charToInt !)
    decode = map (intToChart !)

    next password
        | last password == size charToInt = next (reverse (drop 1 (reverse password))) ++ [1]
        | otherwise = reverse (drop 1 (reverse password)) ++ [last password + 1]

    valid :: [Int] -> Bool
    valid password = 
        let duplicated = length (filter (\a -> length a > 1) $ group $ decode password) >= 2
            notContain = not $ any (`elem` [charToInt ! 'i', charToInt ! 'i', charToInt ! 'l']) password
            increasing = any (\(a, b, c) -> a + 1 == b && b + 1 == c) $ zip3 password (drop 1 password) (drop 2 password)
        in duplicated && notContain && increasing

    main = let p1 = decode $ until valid next (encode "hepxcrrq") in
        print (p1, decode $ until valid next (next $ encode p1))