module Main where
import Data.Map (Map, empty, insertWith, member, lookup, insert, keys, (!), toList)
import Data.List.Split (splitOn)
import Data.List (isPrefixOf, find, sort, sortOn, isInfixOf, permutations)
import Data.Set (Set, empty, union, fromList, null, toList, member, map, filter, take)
import Data.Char (isUpper)
import Data.Maybe (isNothing, fromJust)
import qualified Data.Text as T
import System.Random (newStdGen, RandomGen)
import System.Random.Shuffle (shuffle')

type Instructions = Map String (Set String)
type Possibilites = Set String

main :: IO ()
main = do
    input <- readFile "input.txt"
    rnd <- newStdGen
    let [m,w]= splitOn "\n\n" input
    let (instructions, instructions') = foldl parseMap (Data.Map.empty, Data.Map.empty) $ Prelude.map words $ lines m
    let instructionList = Prelude.map (\(a,b) -> (a, Data.Set.toList b)) (Data.Map.toList instructions')
    let perms = permutations instructionList
    let rndperms = shuffle' instructionList (length instructionList) rnd
    -- nondeterministic lol
    print (length $ getPossibilities instructions w [], fst $ deepReduce rndperms "" (0, w))

parseMap :: (Instructions, Instructions) -> [String] -> (Instructions, Instructions)
parseMap (m,m') [from, "=>", to] = (insertWith Data.Set.union from (fromList [to]) m, insertWith Data.Set.union to (fromList [from]) m')

findReduce :: [[(String, [String])]] -> String -> (Int, String) -> (Int, String)
findReduce [] target from = from
findReduce (x:instructions) target from =
    case deepReduce x "" from of
        (n,t) | t == target -> (n, t)
        (_,t) -> findReduce instructions target from

deepReduce :: [(String, [String])] -> String -> (Int, String) -> (Int, String)
deepReduce instructions last (steps, s) = 
    let (n, t) = reduce instructions (steps, s)
    in if t == last then (n, t) else deepReduce instructions t (n, t)


reduce :: [(String, [String])] -> (Int, String) -> (Int, String)
reduce [] s = s
reduce ((from, [to]):xs) (steps, s) 
    | from `isInfixOf` s = reduce ((from, [to]):xs) ((countOccurrences from s) + steps, (replaceSubstring from to s))
    | otherwise = reduce xs (steps, s)


extendTarget :: Instructions -> String -> Possibilites -> Possibilites
extendTarget instructions target pos = if Data.Set.member target pos then pos else
    foldl (\acc x -> acc `union` getPossibilities instructions x []) Data.Set.empty (Data.Set.take 2000 pos)

getPossibilities :: Instructions -> String -> String -> Possibilites
getPossibilities instructions [] prev = Data.Set.empty
getPossibilities instructions xs prev =
    let coincidence = Prelude.filter (`isPrefixOf` xs) (keys instructions)
    in if Prelude.null coincidence then getPossibilities instructions (tail xs) (prev ++ [head xs]) else
        foldl (\acc x -> acc `union` getPossibilitiesForKey instructions xs x prev) Data.Set.empty coincidence

getNextPossibility' :: Instructions -> String -> String -> String
getNextPossibility' instructions prev [] = prev
getNextPossibility' instructions prev xs =
    let coincidence = find (`isPrefixOf` xs) (keys instructions)
    in if isNothing coincidence then getNextPossibility' instructions (prev ++ [head xs]) (tail xs) else
        getNextPossibility' instructions (prev ++ head (Data.Set.toList (instructions ! fromJust coincidence))) (drop (length $ fromJust coincidence) xs)
    -- in (fromJust coincidence, 0)

getPossibilitiesForKey :: Instructions -> String -> String -> String -> Possibilites
getPossibilitiesForKey instructions xs key prev = Data.Set.map (prev++) (getReplacements instructions key (drop (length key) xs))
    `union` getPossibilities instructions (drop (length key) xs) (prev ++ key)

getReplacements :: Instructions -> String -> String -> Possibilites
getReplacements intructions key t = case Data.Map.lookup key intructions of
    Just x -> Data.Set.map (++ t) x
    Nothing -> Data.Set.empty

replaceSubstring :: String -> String -> String -> String
replaceSubstring old new = T.unpack . T.replace (T.pack old) (T.pack new) . T.pack

countOccurrences :: String -> String -> Int
countOccurrences needle haystack = T.count (T.pack needle) (T.pack haystack)