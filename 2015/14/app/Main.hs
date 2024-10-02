module Main where
import Data.List.Split (splitOn)

data Reindeer = Reindeer {
    speed :: Int, stamina :: Int, rest :: Int,
    flying :: Int, resting :: Int, position :: Int, score :: Int }

main = do
    input <- readFile "input.txt"
    let lines = map (parseLine . splitOn " ") (splitOn "\n" input)
    let finalResult = iterate calculateRound lines !! 2503
    print (maximum $ map position finalResult, maximum $ map score finalResult)

parseLine [_, "can", "fly", speed, "km/s", "for", time, "seconds,", "but", "then", "must", "rest", "for", rest, "seconds."]
    = Reindeer (read speed) (read time) (read rest) (read time) 0 0 0

calculateRound :: [Reindeer] -> [Reindeer]
calculateRound positions = 
    let newPositions = map calc positions
        maxPosition =  maximum $ map position newPositions
    in map (\reindeer -> if position reindeer == maxPosition then reindeer { score = score reindeer + 1} else reindeer) newPositions

calc :: Reindeer -> Reindeer
calc (Reindeer speed stamina rest flying resting position score)
    | flying > 0 = Reindeer speed stamina rest (flying - 1) (if flying - 1 == 0 then rest else 0) (position + speed) score
    | resting > 0 = Reindeer speed stamina rest (if resting - 1 == 0 then stamina else 0) (resting - 1) position score
    | otherwise = Reindeer speed stamina rest stamina rest position score