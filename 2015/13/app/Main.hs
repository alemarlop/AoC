module Main where
import Data.Map (Map, empty, insert, (!), fromList, union)
import Data.Set (Set, empty, insert, toList)
import Data.List.Split (splitOn)
import Data.List (permutations)

type Relationship = Map [String] Int
type People = Set String

getRelationships :: [String] -> (Relationship, People) -> (Relationship, People)
getRelationships [] rels = rels
getRelationships (x:xs) (rels, people) =
    let words = splitOn " " x
    in case words of
        [a, "would", "gain", b, "happiness", "units", "by", "sitting", "next", "to", c] -> 
            getRelationships xs (Data.Map.insert [a,c] (read b) rels, Data.Set.insert a people)
        [a, "would", "lose", b, "happiness", "units", "by", "sitting", "next", "to", c] -> 
            getRelationships xs (Data.Map.insert [a,c] (-1 * read b) rels, Data.Set.insert a people)
        _ -> getRelationships xs (rels, people)

main = do
    input <- readFile "input.txt"
    let (relationships, people) = getRelationships (splitOn ".\n" input) (Data.Map.empty, Data.Set.empty)
    let allPeople = Data.Set.insert "Me" people
    let allRelationships = foldl (\acc x -> Data.Map.insert [x, "Me"] 0 (Data.Map.insert ["Me", x] 0 acc)) relationships allPeople
    print (maximum $ map (calculateHappiness relationships) (permutations (toList people))
        ,maximum $ map (calculateHappiness allRelationships) (permutations (toList allPeople)))

calculateHappiness rels people = foldl (\acc (a,b) -> acc + (rels ! [a,b]) + (rels ! [b,a])) 0 (zip people (tail people ++ [head people]))