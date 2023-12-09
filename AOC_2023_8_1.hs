module AOC_2023_8 where
import Data.List.Split (splitOn)
import System.IO (readFile)
import qualified Data.Map as Map
   
main1 = do  
    contents <- readFile "input.txt"  
    putStrLn $ analyze1 contents

analyze1 :: String -> String
analyze1 s = let s' = filter (/= "") (splitOn "\n" s) in show (count1 (head s') (tail s') "AAA" 0)

count1 :: String ->  [String] -> String -> Int -> Int
count1 str x acc i = let m = Map.fromList (helper x [])
                         rez = f str m acc
                                in if (rez == "ZZZ") then (i + 1) * (length str) else count1 str x rez (i + 1)
                                               

helper :: [String] -> [(String, (String, String))] -> [(String, (String, String))]
helper [] acc             = acc
helper (x : xs) acc       = let x' = filter (\x -> (elem x ['A'..'Z'])) x
                                x1 = take 3 x'
                                x2 = take 3 (drop 3 x')
                                x3 = drop 6 x'
                                        in helper xs ([(x1, (x2, x3))] ++ acc)
                                        
f :: String -> Map.Map String (String, String) -> String -> String
f [] _ acc = acc
f (x : xs) m acc = if (x == 'L') then f xs m (fst (m Map.! acc)) else f xs m (snd (m Map.! acc))