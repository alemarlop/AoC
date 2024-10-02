module Main where
import Text.JSON ( decode, Result (Ok), JSValue (JSRational, JSArray, JSObject) )
import GHC.Real (Ratio, (%))
import Text.JSON.Types

rationalToInt = fromIntegral . truncate

main :: IO ()
main = do
    input <- readFile "input.txt"
    let (Ok a) = decode input :: Result JSValue
    print (processResult (const False) a 0, processResult (JSString (JSONString {fromJSString = "red"}) `elem`) a 0)

processResult :: ([JSValue] -> Bool) -> JSValue -> Int -> Int
processResult _ (JSRational _ n) count = count + rationalToInt n
processResult f (JSArray a) count = count + foldl (\acc x -> processResult f x 0 + acc) 0 a
processResult f (JSObject (JSONObject a)) count =
    let keys = map fst a
        values = map snd a in if f values then count else count + foldl (\acc x -> processResult f (snd x) 0 + acc) 0 a
processResult _ _ count = count
