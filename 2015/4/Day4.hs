module Main where

import Data.List (isPrefixOf)
import Data.ByteString.Char8(pack, unpack, isPrefixOf)
import Data.ByteString.Base16(encode, decode)
import Crypto.Hash.MD5 as MD5 (hash)
import System.Directory.Internal.Prelude (getArgs)


main :: IO ()
main = do
    (prefix: _) <- getArgs
    print $ "Part 1: " ++ show (mine prefix 0 5)
    print $ "Part 2: " ++ show (mine prefix 0 6)
    

mine :: String -> Int -> Int -> Int
mine prefix suffix zeroes = 
    let challenger = prefix ++ show suffix
        hashResult = Main.hash challenger
        target = replicate zeroes '0' in 
    if target `Data.List.isPrefixOf` hashResult then suffix else mine prefix (suffix + 1) zeroes

hash :: String -> String
hash = unpack . encode . MD5.hash . pack