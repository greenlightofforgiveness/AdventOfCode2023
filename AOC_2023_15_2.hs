module AOC_2023_15 where
import Data.List.Split (splitOn)
import Data.Char (ord, isDigit)
import System.IO (readFile)
import qualified Data.Map as Map
   
main2 = do  
    contents <- readFile "input.txt"  
    putStrLn $ analyze2 contents

analyze2 :: String -> String
analyze2 s = show $ count2 (splitOn "," s) (Map.fromList [])

count2 :: [String] -> Map.Map Int (Map.Map String (Int, Int)) -> Int
count2 [] acc         = sum $ map (\(n, m) -> (n + 1) * (sum $ map (\(a, b) -> a * b) (Map.elems m))) (Map.toList acc) 
count2 (x : xs) acc   = let acc' = helper2 x acc in count2 xs acc'

helper2 :: String -> Map.Map Int (Map.Map String (Int, Int)) -> Map.Map Int (Map.Map String (Int, Int))
helper2 x acc   = let   lens_label = takeWhile (\x -> not (elem x ['-', '='])) x
                        box = helper1 lens_label 0
                        focal_length = read (filter (isDigit) x) :: Int
                        operation = if (elem '-' x) then 0 else 1
                        lenses_in_box = if (Map.member box acc) then acc Map.! box else (Map.fromList [])
                        size = Map.size lenses_in_box
                        lenses_in_box' = if ((operation == 0) && (Map.member lens_label lenses_in_box)) then delete_lens lens_label lenses_in_box
                                                else if ((operation == 1) && (Map.member lens_label lenses_in_box)) then replace_lens lens_label focal_length lenses_in_box
                                                        else if (operation == 1) then Map.insert lens_label (focal_length, (size + 1)) lenses_in_box
                                                                else lenses_in_box
                                in Map.insert box lenses_in_box' acc

delete_lens :: String -> Map.Map String (Int, Int) -> Map.Map String (Int, Int)
delete_lens lens_label lenses_in_box = let n = snd (lenses_in_box Map.! lens_label)
                                           m' = Map.delete lens_label lenses_in_box
                                           m'' = Map.map (\(a, b) -> if (b > n) then (a, b - 1) else (a, b)) m'
                                                in m''

replace_lens :: String -> Int -> Map.Map String (Int, Int) -> Map.Map String (Int, Int)
replace_lens lens_label focal_length lenses_in_box = let n = snd (lenses_in_box Map.! lens_label)
                                                         m' = Map.insert lens_label (focal_length, n) lenses_in_box
                                                                in m'                                            
                                
-- "HASH-algorithm"
helper1 :: String -> Int -> Int
helper1 [] acc          = acc
helper1 (x : xs) acc    = helper1 xs (f x acc)

f :: Char -> Int -> Int
f x acc = ((acc + ord x) * 17 ) `mod` 256