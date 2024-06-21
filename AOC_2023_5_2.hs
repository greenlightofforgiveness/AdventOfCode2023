module AOC_2023_5_2 where
import System.IO
import Data.List.Split (splitOn)
import System.IO (readFile)
import qualified Data.Map as Map
import Data.List (find)
import Data.Maybe (fromJust)

{-
Idea: https://aoc-puzzle-solver.streamlit.app/
-}
    
main2 = do  
    contents <- readFile "input.txt"  
    putStrLn $ analyze2 contents

analyze2 :: String -> String
analyze2 s = let x = filter (/= []) $ splitOn "\n" s
                 m = Map.fromListWith (++) (helper (tail x) "" [])
                 seeds = map (\seed -> read seed :: Int) (tail $ splitOn " " $ dropWhile (/= ':') (head x))
                        in show $ count2 (seeds_to_queue seeds []) m []
                        
seeds_to_queue :: [Int] -> [(Int, Int, Int)] -> [(Int, Int, Int)]
seeds_to_queue  [] acc = acc
seeds_to_queue  (x1 : x2 : xs) acc = seeds_to_queue xs (acc ++ [(0, x1, x1 + x2 - 1)])

helper :: [String] -> String -> [(String, [(Int, Int, Int)])] -> [(String, [(Int, Int, Int)])]
helper [] _ acc          = acc
helper (x : xs) k acc    = let f x = [(read (x !! 0) :: Int, read (x !! 1) :: Int, read (x !! 2) :: Int)]
                                in if (elem ':' x) then helper xs (takeWhile (/=':') x) acc else helper xs k ([(k, f (splitOn " " x))] ++ acc)
                                
count2 :: [(Int, Int, Int)] -> Map.Map String [(Int, Int, Int)] -> [Int] -> Int
count2 [] m acc         = minimum acc
count2 ((a, b, c) : xs) m acc   | a == 7 = count2 xs m (b : acc)
                                | a == 0 = let todo = conversion (a, b, c) (m Map.! "seed-to-soil map") in count2 (todo ++ xs) m acc
                                | a == 1 = let todo = conversion (a, b, c) (m Map.! "soil-to-fertilizer map") in count2 (todo ++ xs) m acc
                                | a == 2 = let todo = conversion (a, b, c) (m Map.! "fertilizer-to-water map") in count2 (todo ++ xs) m acc
                                | a == 3 = let todo = conversion (a, b, c) (m Map.! "water-to-light map") in count2 (todo ++ xs) m acc
                                | a == 4 = let todo = conversion (a, b, c) (m Map.! "light-to-temperature map") in count2 (todo ++ xs) m acc
                                | a == 5 = let todo = conversion (a, b, c) (m Map.! "temperature-to-humidity map") in count2 (todo ++ xs) m acc
                                | a == 6 = let todo = conversion (a, b, c) (m Map.! "humidity-to-location map") in count2 (todo ++ xs) m acc
                                
conversion :: (Int, Int, Int) -> [(Int, Int, Int)] -> [(Int, Int, Int)]
conversion (a, b, c) intervals  | v1 /= Nothing = let (x, y, z) = fromJust v1 in [(a + 1, b - y + x, c - y + x)]
                                | v2 /= Nothing = let (x, y, z) = fromJust v2 in [(a + 1, x, x + z - 1), (a, b, y - 1), (a, y + z, c)]
                                | v3 /= Nothing = let (x, y, z) = fromJust v3 in [(a + 1, b - y + x, x + z - 1), (a, y + z, c)]
                                | v4 /= Nothing = let (x, y, z) = fromJust v4 in [(a + 1, x, c - y + x), (a, b, y - 1)]
                                | otherwise = [(a + 1, b, c)]
                                        where v1 = find (\(x, y, z) -> (y <= b) && (c <= (y + z - 1))) intervals
                                              v2 = find (\(x, y, z) -> (y > b) && (c > (y + z - 1))) intervals
                                              v3 = find (\(x, y, z) -> (y < b) && (c > (y + z - 1)) && (b < (y + z - 1)) && (y < c)) intervals
                                              v4 = find (\(x, y, z) -> (y > b) && (c < (y + z - 1)) && (b < (y + z - 1)) && (y < c)) intervals