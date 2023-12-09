module AOC_2023_9 where
import Data.List.Split (splitOn)
import System.IO (readFile)
   
main1 = do  
    contents <- readFile "input.txt"  
    putStrLn $ analyze1 contents

analyze1 :: String -> String
analyze1 s = show (count1 (splitOn "\n" s) [])

main2 = do  
    contents <- readFile "input.txt"  
    putStrLn $ analyze2 contents

analyze2 :: String -> String
analyze2 s = show (count2 (splitOn "\n" s) [])

count1 :: [String] -> [Int] -> Int
count1 [] acc = sum acc
count1 (x : xs) acc = let x' = map (\x -> read x :: Int) (splitOn " " x)
                          delta = helper1 x' [] ++ [last x']
                                in count1 xs (acc ++ [sum1 delta])

helper :: [Int] -> [Int] -> [Int]
helper [] acc             = acc
helper [x1 , x2] acc      = helper [] (acc ++ [(x2 - x1)])
helper (x1 : x2 : xs) acc = helper ([x2] ++ xs) (acc ++ [(x2 - x1)])

helper1 :: [Int] -> [Int] -> [Int]
helper1 x acc = let deltas = helper x []
                        in if (allTheSame deltas) then ([deltas !! 0] ++ acc) else helper1 deltas ([last deltas] ++ acc)


{- https://stackoverflow.com/questions/31036474/haskell-checking-if-all-list-elements-are-unique -}
allTheSame :: (Eq a) => [a] -> Bool
allTheSame list = case list of
    []        -> True
    [x]       -> True
    (x : xs)  -> x `elem` xs && allTheSame xs
    
sum1 :: [Int] -> Int
sum1 [] = 0
sum1 [x] = x
sum1 [x1, x2] = x1 + x2
sum1 (x1 : x2 : xs) = sum1 ([x1 + x2] ++ xs)

   
count2 :: [String] -> [Int] -> Int
count2 [] acc = sum acc
count2 (x : xs) acc = let x' = map (\x -> read x :: Int) (splitOn " " x)
                          delta = helper2 x' [] ++ [head x']
                                in count2 xs (acc ++ [sum2 delta])


helper2 :: [Int] -> [Int] -> [Int]
helper2 x acc = let deltas = helper x []
                        in if (allTheSame deltas) then ([deltas !! 0] ++ acc) else helper2 deltas ([head deltas] ++ acc)
    
sum2 :: [Int] -> Int
sum2 [] = 0
sum2 [x] = x
sum2 [x1, x2] = x2 - x1
sum2 (x1 : x2 : xs) = sum2 ([x2 - x1] ++ xs)