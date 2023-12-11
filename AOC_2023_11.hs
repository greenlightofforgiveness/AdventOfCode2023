module AOC_2023_11 where
import Data.List.Split (splitOn)
import Data.List (elemIndices)
import System.IO (readFile)
   
main = do  
    contents <- readFile "input.txt"  
    putStrLn $ analyze contents

analyze :: String -> String
analyze s = let s' = splitOn "\n" s
                f_c = free_columns s' [0 .. (length (s' !! 0) - 1)]
                f_r = free_rows s' [0 .. (length s' - 1)]
                g_i = gal_indices s' 0 []
                        in show $ count g_i f_c f_r

count :: [(Int, Int)] -> [Int] -> [Int] -> Int
count g_i f_c f_r = sum (map (\(a, b) -> dist a b f_c f_r) [((x1, y1), (x2, y2)) | (x1, y1) <- g_i, (x2, y2) <- g_i, x1 <= x2, not ((x1 == x2) && (y1 == y2)), not ((y1 > y2) && (x1 == x2))])

dist :: (Int, Int) -> (Int, Int) -> [Int] -> [Int] -> Int
dist (x1, y1) (x2, y2) f_c f_r = let d1 = length (filter (\x -> ((x > x1) && (x < x2))) f_r)
                                     d2 = length (filter (\y -> (y > (min y1 y2)) && (y < (max y1 y2))) f_c)
                                        in (x2 - x1) + abs (y2 - y1) + (d1 + d2) * 999999 -- For part 1 (* 1)
                                                
free_columns :: [String] -> [Int] -> [Int]
free_columns [] acc = acc
free_columns (x : xs) acc = free_columns xs (filter (/= (-1)) $ map (\i -> if ((x !! i) == '.') then i else (-1)) acc)

free_rows :: [String] -> [Int] -> [Int]
free_rows x acc =  filter (\j -> (all (== '.' ) (x !! j))) acc

gal_indices :: [String] -> Int -> [(Int, Int)] -> [(Int, Int)]
gal_indices [] _ acc = acc
gal_indices (x : xs) n acc = gal_indices xs (n + 1) (acc ++ [(i, j) | i <- [n], j <- (elemIndices '#' x)])