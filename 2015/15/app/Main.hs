module Main where
import Data.List.Split (splitOn)

type Quantities = (Int, Int, Int, Int)
data Ingredient = Ingredient { capacity :: Int, durability :: Int, flavor :: Int, texture :: Int, calories :: Int }

main = do
    input <- readFile "input.txt"
    let lines = map (getIngredientValue .splitOn " ") (splitOn "\n" input)
    let combinations = [(a, b, c, d) | a <- [0..100], b <- [0..100], c <- [0..100], d <- [0..100], a + b + c + d == 100]
    print (maximum $ map (getScore lines (const True)) combinations, maximum $ map (getScore lines (== 500)) combinations)

getIngredientValue :: [String] -> Ingredient
getIngredientValue [_, _, capacity, _, durability, _, flavor, _, texture, _, calories] =
    Ingredient (read $ init capacity) (read $ init durability) (read $ init flavor) (read $ init texture) (read calories)

getScore :: [Ingredient] -> (Int -> Bool) -> Quantities -> Int
getScore (i1:i2:i3:i4:_) f (a, b, c, d) =
    let cap = max 0 $ (capacity i1 * a) + (capacity i2 * b) + (capacity i3 * c) + (capacity i4 * d)
        dur = max 0 $ (durability i1 * a) + (durability i2 * b) + (durability i3 * c) + (durability i4 * d)
        fla = max 0 $ (flavor i1 * a) + (flavor i2 * b) + (flavor i3 * c) + (flavor i4 * d)
        tex = max 0 $ (texture i1 * a) + (texture i2 * b) + (texture i3 * c) + (texture i4 * d)
        cal = (calories i1 * a) + (calories i2 * b) + (calories i3 * c) + (calories i4 * d)
    in if f cal then cap * dur * fla * tex else 0