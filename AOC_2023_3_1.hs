module AOC_2023_3 where 
import Data.List.Split (splitWhen, splitOn)
import Data.Char (isDigit)
import System.IO (readFile)
import Data.List (findIndices, isPrefixOf, tails)
    
main1 = do  
    contents <- readFile "input.txt"  
    putStrLn $ analyze1 contents
    
analyze1 :: String -> String
analyze1 s = show (count1 ([""] ++ (splitOn "\n" s)) 0)

count1 :: [String] -> Int -> Int
count1 [] acc           = acc
count1 [x1, x2] acc     = count1 [x1, x2, ""] acc 
count1 (x1 : x2 : x3 : xs) acc     = let nums = filter (/= "") $ splitWhen (not . isDigit) x2
                                         nums_indices = helper nums x2 0 []
                                         nums_l = map length nums
                                         acc' = sum_good_nums nums nums_indices nums_l x1 x2 x3 0
                                                in if (x3 /= "") then count1 ([x2, x3] ++ xs) (acc + acc') else (acc + acc')

{- https://stackoverflow.com/questions/48198144/how-do-i-search-for-string-within-a-string-in-haskell -}
findString :: (Eq a) => [a] -> [a] -> [Int]
findString search str = findIndices (isPrefixOf search) (tails str)

helper :: [String] -> String -> Int -> [Int] -> [Int]
helper [] _ _ acc2             = acc2
helper (x : xs) str acc1 acc2  = let x' = findString x str
                                     acc1' = (x'!!0) + (length x)
                                     str' = drop acc1' str
                                      in helper xs str' (acc1 + acc1') (acc2 ++ [x'!!0 + acc1])
                                      
sum_good_nums :: [String] -> [Int] -> [Int] -> String -> String -> String -> Int -> Int
sum_good_nums [] [] [] _ _ _ acc                 = acc
sum_good_nums (x : xs) (y : ys) (z : zs) s1 s2 s3 acc  = let str1 = if (y /= 0) then [s2 !! (y - 1)] else ""
                                                             str2 = if (y + z <= (length s2 - 1)) then [s2 !! (y + z)] else ""
                                                             str3 = if (y /= 0) then take (z + 2) (drop (y - 1) s1) else take (z + 1) s1
                                                             str4 = if (y /= 0) then take (z + 2) (drop (y - 1) s3) else take (z + 1) s3
                                                             str = filter (not . isDigit) (filter (/='.') (str1 ++ str2 ++ str3 ++ str4))
                                                             acc' = if ((length str) /= 0) then acc + (read x :: Int) else acc
                                                                    in sum_good_nums xs ys zs s1 s2 s3 acc'