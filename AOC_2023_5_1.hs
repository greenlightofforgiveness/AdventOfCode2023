module AOC_2023_5_1 where
import System.IO
import Data.List.Split (splitOn)
import System.IO (readFile)
import qualified Data.Map as Map
import Data.List (find)
import Data.Maybe (fromJust)
    
main1 = do  
    contents <- readFile "input.txt"  
    putStrLn $ analyze1 contents

analyze1 :: String -> String
analyze1 s = let x = filter (/= []) $ splitOn "\n" s
                 m = Map.fromListWith (++) (helper (tail x) "" [])
                 seeds = map (\seed -> read seed :: Int) (tail $ splitOn " " $ dropWhile (/= ':') (head x))
                        in show $ count1 seeds m []

count1 :: [Int] -> Map.Map String [(Int, Int, Int)] -> [Int] -> Int
count1 [] m acc         =  minimum acc
count1 (x : xs) m acc   = let   soil' = find (\(a, b, c) -> (x >= b) && (x < b + c)) (m Map.! "seed-to-soil map")
                                soil = if (soil' /= Nothing) then let (a, b, c) = fromJust soil' in a + x - b else x
                                fert' = find (\(a, b, c) -> (soil >= b) && (soil < b + c)) (m Map.! "soil-to-fertilizer map")
                                fert = if (fert' /= Nothing) then let (a, b, c) = fromJust fert' in a + soil - b else soil
                                water' = find (\(a, b, c) -> (fert >= b) && (fert < b + c)) (m Map.! "fertilizer-to-water map")
                                water = if (water' /= Nothing) then let (a, b, c) = fromJust water' in a + fert - b else fert
                                light' = find (\(a, b, c) -> (water >= b) && (water < b + c)) (m Map.! "water-to-light map")
                                light = if (light' /= Nothing) then let (a, b, c) = fromJust light' in a + water - b else water
                                temp' = find (\(a, b, c) -> (light >= b) && (light < b + c)) (m Map.! "light-to-temperature map")
                                temp = if (temp' /= Nothing) then let (a, b, c) = fromJust temp' in a + light - b else light
                                hum' = find (\(a, b, c) -> (temp >= b) && (temp < b + c)) (m Map.! "temperature-to-humidity map")
                                hum = if (hum' /= Nothing) then let (a, b, c) = fromJust hum' in a + temp - b else temp
                                loc' = find (\(a, b, c) -> (hum >= b) && (hum < b + c)) (m Map.! "humidity-to-location map")
                                loc = if (loc' /= Nothing) then let (a, b, c) = fromJust loc' in a + hum - b else hum
                                        in count1 xs m (acc ++ [loc])

helper :: [String] -> String -> [(String, [(Int, Int, Int)])] -> [(String, [(Int, Int, Int)])]
helper [] _ acc          = acc
helper (x : xs) k acc    = let f x = [(read (x !! 0) :: Int, read (x !! 1) :: Int, read (x !! 2) :: Int)]
                                in if (elem ':' x) then helper xs (takeWhile (/=':') x) acc else helper xs k ([(k, f (splitOn " " x))] ++ acc)
