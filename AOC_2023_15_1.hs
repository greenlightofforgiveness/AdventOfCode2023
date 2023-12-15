module AOC_2023_15 where
import Data.List.Split (splitOn)
import Data.Char (ord)
import System.IO (readFile)
   
main1 = do  
    contents <- readFile "input.txt"  
    putStrLn $ analyze1 contents

analyze1 :: String -> String
analyze1 s = show (count1 (splitOn "," s) 0)

count1 :: [String] -> Int -> Int
count1 [] acc          = acc
count1 (x : xs) acc    = let acc' = helper x 0 in count1 xs (acc + acc')

helper :: String -> Int -> Int
helper [] acc          = acc
helper (x : xs) acc    = helper xs (f x acc)

f :: Char -> Int -> Int
f x acc = ((acc + ord x) * 17 ) `mod` 256