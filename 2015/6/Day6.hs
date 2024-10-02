module Main where
import System.Directory.Internal.Prelude (getArgs)
import Data.List.Split (splitOn)
import Data.Set (Set, empty, union, fromList, difference, intersection, size, foldl, toList)
import Data.Map (Map, empty, fromList, elems, adjust, (!), member, insert)
import Text.Regex.TDFA ( (=~), getAllTextMatches )
import Data.List ( isPrefixOf, (\\), group, sort )

data Action = TurnOn | TurnOff | Toggle deriving Show
data Input = Input { action :: Action, from :: (Int, Int), to :: (Int, Int) } deriving Show
newtype Cell = Cell { coordinates :: (Int, Int) } deriving (Eq, Ord)

main :: IO ()
main = do
    (fileName:_) <- getArgs
    fileContent <- readFile fileName
    let lines :: [String] = splitOn "\n" fileContent
        parsedLines :: [Input] = reverse $ parseLines lines []
    print $ "Part 1: " ++ show (play parsedLines Data.Set.empty)
    print $ "Part 2: " ++ show (playPart2 parsedLines Data.Map.empty)

play :: [Input] -> Set Cell -> Int
play [] onCells = size onCells
play (x:xs) onCells = play xs (playRound x onCells)

playPart2 :: [Input] -> Map Cell Int -> Int
playPart2 [] onCells = sum (Data.Map.elems onCells)
playPart2 (x:xs) onCells = playPart2 xs $ playRoundPart2 x onCells

playRound :: Input -> Set Cell -> Set Cell
playRound action onCells = case action of
    Input TurnOn from to -> onCells `union` getRange from to
    Input TurnOff from to -> onCells `difference` getRange from to
    Input Toggle from to -> toggle onCells $ getRange from to

playRoundPart2 :: Input -> Map Cell Int -> Map Cell Int
playRoundPart2 action grid = case action of
    Input TurnOn from to -> updateMapPart2 (Data.Set.toList (getRange from to)) grid 1
    Input TurnOff from to -> updateMapPart2 (Data.Set.toList (getRange from to)) grid (-1)
    Input Toggle from to -> updateMapPart2 (Data.Set.toList (getRange from to)) grid 2

updateMapPart2 :: [Cell] -> Map Cell Int -> Int -> Map Cell Int
updateMapPart2 [] map _ = map
updateMapPart2 (x:s) map n
  | Data.Map.member x map = updateMapPart2 s (Data.Map.adjust (\val -> max (val+n) 0) x map) n
  | n > 0 = updateMapPart2 s (Data.Map.insert x (max n 0) map) n
  | otherwise = updateMapPart2 s map n

updateMap :: [Cell] -> Map Cell Int -> Int -> Map Cell Int
updateMap [] map _ = map
updateMap (x:xs) map n = let num = if map ! x == 0 && n < 0 then 0 else n in
    updateMap xs (Data.Map.adjust (+num) x map) n

toggle :: Set Cell -> Set Cell -> Set Cell
toggle on activated =
    let toTurnOn = activated `difference` on
        toTurnOff = activated `intersection` on in
    (on `difference` toTurnOff) `union` toTurnOn

getRange :: (Int, Int) -> (Int, Int) -> Set Cell
getRange (fX, fY) (tX, tY) =
    let xRange = [min fX tX..max fX tX]
        yRange = [min fY tY..max fY tY] in
    Data.Set.fromList [Cell (x,y) | x <- xRange, y <- yRange]



parseLines :: [String] -> [Input] -> [Input]
parseLines xs inputs = Prelude.foldl (\ inputs x -> parseLine x : inputs) inputs xs

parseLine :: String -> Input
parseLine line =
    let numPattern = "[0-9]+"
        coordinates = parseNumbers (getAllTextMatches (line =~ numPattern) :: [String])
        action
            | "turn on" `isPrefixOf` line = TurnOn
            | "turn off" `isPrefixOf` line = TurnOff
            | otherwise = Toggle
        in
    uncurry (Input action) coordinates


parseNumbers :: [String] -> ((Int, Int), (Int, Int))
parseNumbers (u:v:x:y:_) = ((read u :: Int, read v :: Int), (read x :: Int, read y :: Int))

initPart2 :: Map Cell Int
initPart2 = Data.Map.fromList [(Cell (x,y), 0) | x <- [0..999], y <- [0..999]]