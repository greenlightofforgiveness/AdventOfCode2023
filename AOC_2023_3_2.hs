module AOC_2023_3 where 
import Data.List.Split (splitWhen, splitOn)
import Data.Char (isDigit)
import System.IO (readFile)
import Data.List (findIndices, isPrefixOf, tails)
import qualified Data.Map as Map
    
main2 = do  
    contents <- readFile "input.txt"  
    putStrLn $ analyze2 contents
    
analyze2 :: String -> String
analyze2 s = let l = count2 ([""] ++ (splitOn "\n" s)) 0 []
                 m = Map.fromListWith (++) l 
                 f x acc = if (length x > 1) then acc + (foldl (*) 1 x) else acc in show (Map.fold f 0 m)

count2 :: [String] -> Int -> [(String, [Int])] -> [(String, [Int])]
count2 [] _ acc           = acc
count2 [x1, x2] i acc     = count2 [x1, x2, ""] i acc 
count2 (x1 : x2 : x3 : xs) i acc     = let nums = filter (/= "") $ splitWhen (not . isDigit) x2
                                           nums_indices = helper nums x2 0 []
                                           nums_l = map length nums
                                           acc' = sum_good_nums nums nums_indices nums_l x1 x2 x3 i []
                                                in if (x3 /= "") then count2 ([x2, x3] ++ xs) (i + 1) (acc ++ acc') else (acc ++ acc')

{- https://stackoverflow.com/questions/48198144/how-do-i-search-for-string-within-a-string-in-haskell -}
findString :: (Eq a) => [a] -> [a] -> [Int]
findString search str = findIndices (isPrefixOf search) (tails str)

helper :: [String] -> String -> Int -> [Int] -> [Int]
helper [] _ _ acc2             = acc2
helper (x : xs) str acc1 acc2  = let x' = findString x str
                                     acc1' = (x'!!0) + (length x)
                                     str' = drop acc1' str
                                        in helper xs str' (acc1 + acc1') (acc2 ++ [x'!!0 + acc1])
                                      
sum_good_nums :: [String] -> [Int] -> [Int] -> String -> String -> String -> Int -> [(String, [Int])] -> [(String, [Int])]
sum_good_nums [] [] [] _ _ _ _ acc                 = acc
sum_good_nums (x : xs) (y : ys) (z : zs) s1 s2 s3 i acc  = let  str1 = if (y /= 0) then [s2 !! (y - 1)] else ""
                                                                str2 = if (y + z <= (length s2 - 1)) then [s2 !! (y + z)] else ""
                                                                str3 = if (y /= 0) then take (z + 2) (drop (y - 1) s1) else take (z + 1) s1
                                                                str4 = if (y /= 0) then take (z + 2) (drop (y - 1) s3) else take (z + 1) s3
                                                                gear1 = if (str1 == "*") then ((show i) ++ " " ++ show (y - 1)) else ""
                                                                gear2 = if (str2 == "*") then ((show i) ++ " " ++ show (y + z)) else ""
                                                                gears3 = let y' = if (y > 0) then y - 1 else 0 in map (\s -> (show (i - 1)) ++ " " ++ (show s)) (map (+ y') $ findString "*" str3)
                                                                gears4 = let y' = if (y > 0) then y - 1 else 0 in map (\s -> (show (i + 1)) ++ " " ++ (show s)) (map (+ y') $ findString "*" str4)
                                                                gears = (filter (/= "") [gear1, gear2]) ++ gears3 ++ gears4
                                                                acc' = acc ++ (map (\g -> (g, [read x :: Int])) gears)
                                                                        in sum_good_nums xs ys zs s1 s2 s3 i acc'