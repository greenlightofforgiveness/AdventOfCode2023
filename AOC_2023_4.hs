module AOC_2023_4 where
import System.IO     
import Data.List.Split (splitOn)
import Data.Char (isDigit)
import System.IO (readFile)
import qualified Data.Map as Map
    
main1 = do  
    contents <- readFile "input.txt"  
    putStrLn $ analyze1 contents

analyze1 :: String -> String
analyze1 s = show (count1 (splitOn "\n" s) 0)

main2 = do  
    contents <- readFile "input.txt"  
    putStrLn $ analyze2 contents

analyze2 :: String -> String
analyze2 s = let m = Map.fromList $ helper (splitOn "\n" s) []
                 max_n = fst (Map.findMax m)
                        in show (count2 m [1 .. max_n])

count1 :: [String] -> Int -> Int
count1 [] acc          = acc
count1 (x : xs) acc    = let x' = splitOn "|" ((splitOn (": ") x) !! 1)
                             x1 = filter (/= "") $ splitOn (" ") (x' !! 0)
                             x2 = filter (/= "") $ splitOn (" ") (x' !! 1)
                             s = sum $ map (\x -> if (elem x x1) then 1 else 0) x2
                                in if (s > 0) then count1 xs (acc + 2^(s - 1)) else count1 xs acc

count2 :: Map.Map Int Int -> [Int] -> Int
count2 _ [] = 0
count2 m cards = let max_n = fst (Map.findMax m)
                     cards' = concatMap (\x -> if (x <= max_n) && ((x + (m Map.! x)) <= max_n) then [(x + 1) .. (x + (m Map.! x))] else []) cards in (length cards) + count2 m cards'

helper :: [String] -> [(Int, Int)] -> [(Int, Int)]
helper [] acc          = acc
helper (x : xs) acc    = let n = read (filter (isDigit) $ takeWhile (/= ':') x) :: Int
                             x' = splitOn "|" ((splitOn (": ") x) !! 1)
                             x1 = filter (/= "") $ splitOn (" ") (x' !! 0)
                             x2 = filter (/= "") $ splitOn (" ") (x' !! 1)
                             s = sum $ map (\x -> if (elem x x1) then 1 else 0) x2
                                in helper xs (acc ++ [(n, s)])