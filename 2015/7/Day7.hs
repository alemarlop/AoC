module Main where

    import Data.Bits (Bits (shiftL, shiftR, complement, (.|.), (.&.)))
    import Data.Map (Map, insert, empty, member, (!))
    import System.Directory.Internal.Prelude ()
    import Data.List.Split ( splitOn )
    import Text.Read (readMaybe)
    import Data.Maybe (isJust, fromJust)

    type Value = String
    data Operand = AND | OR | LSHIFT | RSHIFT | NOT | VALUE
    data Operation = Binary Value Operand Value Value | Unary Operand Value Value

    main :: IO ()
    main = do
        input <- readFile "input.txt"
        input2 <- readFile "input2.txt"
        let [lines, lines2] :: [[Operation]] = map (map parseLine . splitOn "\n") [input, input2]
            result = runAndConverge lines empty
            result2 = runAndConverge lines2 empty
        print $ "Part 1: " ++ show (result ! "a")
        print $ "Part 2: " ++ show (result2 ! "a")

    runAndConverge :: [Operation] -> Map Value Int -> Map Value Int
    runAndConverge ops m =
        let msize = length m
            m' = run ops m in
        if msize == length m' then m' else runAndConverge ops m'

    run :: [Operation] -> Map Value Int -> Map Value Int
    run [] m = m
    run (Binary a op b c:xs) m =
        let a' :: Maybe Int = if member a m then Just (m ! a) else readMaybe a :: Maybe Int
            b' :: Maybe Int = if member b m then Just (m ! b) else readMaybe b :: Maybe Int in
        run xs $
        if isJust a' && isJust b' then
            case op of
                AND -> insert c (fromJust b' .&. fromJust a') m
                OR -> insert c (fromJust a' .|. fromJust b') m
                LSHIFT -> insert c (fromJust a' `shiftL` fromJust b') m
                RSHIFT -> insert c (fromJust a' `shiftR` fromJust b') m
        else m
    run (Unary op a b:xs) m =
        let a' :: Maybe Int = if member a m then Just (m ! a) else readMaybe a :: Maybe Int in
        run xs $
        if isJust a' then
            case op of
                NOT -> insert b (complement16bits (fromJust a')) m
                VALUE -> insert b (fromJust a') m
        else m

    complement16bits :: Int -> Int
    complement16bits num = complement num .&. ((1 `shiftL` 16) - 1)

    parseLine :: String -> Operation
    parseLine line = case splitOn " " line of
        [a, "AND", b, "->", c] -> Binary a AND b c
        [a, "OR", b, "->", c] -> Binary a OR b c
        [a, "LSHIFT", b, "->", c] -> Binary a LSHIFT b c
        [a, "RSHIFT", b, "->", c] -> Binary a RSHIFT b c
        ["NOT", a, "->", b] -> Unary NOT a b
        [a, "->", b] -> Unary VALUE a b