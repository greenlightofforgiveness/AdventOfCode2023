module AOC_2023_5 where
import System.IO     
import Data.List.Split (splitOn)
import System.IO (readFile)
import qualified Data.Map as Map
    
main1 = do  
    contents <- readFile "input.txt"  
    putStrLn $ analyze1 contents

analyze1 :: String -> String
analyze1 s = let x = filter (/= []) $ splitOn "\n" s
                 m = Map.fromListWith (++) (helper (tail x) "" [])
                 seeds = map (\x -> read x :: Int) (tail $ splitOn " " $ dropWhile (/= ':') (head x))
                        in show $ count1 seeds m []

count1 :: [Int] -> Map.Map String [(Int, Int, Int)] -> [Int] -> Int
count1 [] m acc 	  =  minimum acc
count1 (x : xs) m acc = let f n (x, y, z) = if ((n >= y) && (n <= y + z)) then (x + n - y) else (-1);
							soil' = filter (/= (-1)) $ map (f x) (m Map.! "seed-to-soil map");
							soil = if (soil' /= []) then soil'!!0 else x;
							fert' = filter (/= (-1)) $ map (f soil) (m Map.! "soil-to-fertilizer map");
							fert = if (fert' /= []) then fert'!!0 else soil;
							water' = filter (/= (-1)) $ map (f fert) (m Map.! "fertilizer-to-water map");
							water = if (water' /= []) then water'!!0 else fert;
							light' = filter (/= (-1)) $ map (f water) (m Map.! "water-to-light map");
							light = if (light' /= []) then light'!!0 else water;		
							temp' = filter (/= (-1)) $ map (f light) (m Map.! "light-to-temperature map");
							temp = if (temp' /= []) then temp'!!0 else light;
							hum' = filter (/= (-1)) $ map (f temp) (m Map.! "temperature-to-humidity map");
							hum = if (hum' /= []) then hum'!!0 else temp;
							loc' = filter (/= (-1)) $ map (f hum) (m Map.! "humidity-to-location map");
							loc = if (loc' /= []) then loc'!!0 else hum;
								in count1 xs m (acc ++ [loc])

helper :: [String] -> String -> [(String, [(Int, Int, Int)])] -> [(String, [(Int, Int, Int)])]
helper [] _ acc          = acc
helper (x : xs) k acc    = let f x = [(read (x!!0) :: Int, read (x!!1) :: Int, read (x!!2) :: Int)]
								in if (elem ':' x) then helper xs (takeWhile (/=':') x) acc else helper xs k ([(k, f (splitOn " " x))] ++ acc)