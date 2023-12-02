module AOC_2023_1 where
import Data.List.Split (splitOn)
import Data.Char (isDigit)
import System.IO (readFile)
import Data.List (isPrefixOf, findIndices, tails, elemIndex)
   
main1 = do  
    contents <- readFile "input.txt"  
    putStrLn $ analyze1 contents

main2 = do  
    contents <- readFile "input.txt"  
    putStrLn $ analyze2 contents 
    
analyze1 :: String -> String
analyze1 s = show (count1 (splitOn "\n" s) 0)

analyze2 :: String -> String
analyze2 s = show (count2 (splitOn "\n" s) 0)                              

count1 :: [String] -> Int -> Int
count1 [] a             = a
count1 (x : xs) a       = let k = filter isDigit x in count1 xs (a + (read ([k !! 0] ++ [k !! ((length k) - 1)]) :: Int))

digits = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

{- https://stackoverflow.com/questions/48198144/how-do-i-search-for-string-within-a-string-in-haskell -}
findString :: (Eq a) => [a] -> [a] -> [Int]
findString search str = findIndices (isPrefixOf search) (tails str)

{- https://www.educative.io/answers/how-to-check-if-an-element-is-present-in-a-list-in-haskell -}
contains :: Eq a => a -> [a] -> Bool
contains = \elem -> \myList ->
  case myList of
    [] -> False -- if all elements checked, return False
    x:xs | x == elem -> True -- If head matches elem, return True
    _:xs -> contains elem xs -- Check for element membership in remaining list

-- Template, four possible digits: x2 (digit) x1 (text) y1 (text) y2 (digit)    
count2 :: [String] -> Int -> Int
count2 [] a             = a
count2 (x : xs) a       = let k = map (\q -> findString q x) digits
                              index1 = if (filter (/= []) k /= []) then minimum $ filter (/= (-1)) $ (map (\x -> if (x /= []) then minimum x else (-1)) k) else (-1)
                              index2 = if (filter (/= []) k /= []) then maximum $ filter (/= (-1)) $ (map (\x -> if (x /= []) then maximum x else (-1)) k) else (-1)
                              x1 = if (index1 /= (-1)) then (findIndices (contains index1) k) !! 0 + 1 else 0
                              y1 = if (index2 /= (-1)) then (findIndices (contains index2) k) !! 0 + 1  else 0
                              x2' = if (x1 > 0) then filter isDigit (take (index1) x) else ""
                              y2' = if (y1 > 0) then filter isDigit (drop (index2 + 1) x) else ""
                              x2 = if (x2' /= "") then read ([head x2']) :: Int else 0
                              y2 = if (y2' /= "") then read ([last y2']) :: Int else 0
                              s' = if (x2 /= 0) then x2 * 10 else if (((y1 /= 0) && (index1 /= index2)) || ((y2 /= 0) && (index1 == index2))) then x1 * 10 else 0
                              s =  if (y2 /= 0) then y2 + s' else y1 + s'
                              m = filter isDigit x
                              s'' = if (m /= []) && (length m /= 1) then (read [head m]::Int)*10 + (read [last m]::Int) else if (length m == 1) then ((read [head m]::Int) * 11) else 0
                              s_f = if (index1 /= (-1)) then s else s''
                                                in count2 xs (a + s_f)