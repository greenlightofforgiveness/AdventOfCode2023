module AOC_2023_7 where
import Data.List.Split (splitOn)
import System.IO (readFile)
import Data.List (permutations)
import qualified Data.Map as Map (keys, elems, fromList)
   
main1 = do  
    contents <- readFile "input.txt"  
    putStrLn $ analyze1 contents

analyze1 :: String -> String
analyze1 s = show (count1 (splitOn "\n" s) 0)

cards = ['A', 'K', 'Q', 'J', 'T', '9', '8', '7', '6', '5', '4', '3', '2']

hand_type :: String -> Int
{-
Five of a kind, where all five cards have the same label: AAAAA, 7
Four of a kind, where four cards have the same label and one card has a different label: AA8AA, 6
Full house, where three cards have the same label, and the remaining two cards share a different label: 23332, 5
Three of a kind, where three cards have the same label, and the remaining two cards are each different from any other card in the hand: TTT98, 4
Two pair, where two cards share one label, two other cards share a second label, and the remaining card has a third label: 23432,3
One pair, where two cards share one label, and the other three cards have a different label from the pair and each other: A23A4, 2
High card, where all cards' labels are distinct: 23456, 1
-}
hand_type x  | length (f x) == 5 = 1
             | length (f x) == 4 = 2
             | elem (f x) (permutations [2,2,1]) = 3
             | elem (f x) (permutations [3,1,1]) = 4
             | (f x == [2,3]) || (f x == [3,2]) = 5
             | (f x == [1,4]) || (f x == [4,1]) = 6
             | length (f x) == 1 = 7
             | otherwise = 0
                where f x = filter (/= 0) (map length (map (\c -> filter (== c) x) cards))

helper :: [String] -> [[(String, Int)]] -> [[(String, Int)]]
helper [] acc = acc
helper (x : xs) acc = let x' = splitOn " " x
                          k = x' !! 0
                          n = read (x' !! 1) :: Int
                          t' = (hand_type k) - 1
                          acc1 = (map (\x -> if (x /= t') then (acc !! x) else []) [0 .. t' - 1])
                          acc2 = [((acc !! t') ++ [((replace k), n)])] 
                          acc3 = (map (\x -> if (x /= t') then (acc !! x) else []) [t' + 1 .. 6])
                          acc' = acc1 ++ acc2 ++ acc3 
                                in helper xs acc'

count1 :: [String] -> Int -> Int
count1 x acc = let h = filter (/= []) (helper x (take 7 $ repeat []))
                   maps = map (Map.fromList) h
                   keys = concatMap Map.keys maps
                   elems = concatMap Map.elems maps
                        in sum $ map (\(a, b) -> a * b) $ zip elems [1 .. (length elems)]
 
replace :: String -> String
replace x = map f x
                where f c | c == 'T' = 'A'
                          | c == 'J' = 'B'
                          | c == 'Q' = 'C'
                          | c == 'K' = 'D'
                          | c == 'A' = 'E' 
                          | otherwise = c