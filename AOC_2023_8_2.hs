module AOC_2023_8 where
import Data.List.Split (splitOn)
import System.IO (readFile)
import qualified Data.Map as Map
   
main1 = do  
    contents <- readFile "input.txt"  
    putStrLn $ analyze1 contents

analyze1 :: String -> String
analyze1 s = let s' = filter (/= "") (splitOn "\n" s)
                 s1 = head s'
                 s2 = tail s'
                 s'' = f' s2 []
                 s''' = f'' s2 []
                        in show $ lcmm $ filter (/= (-1)) $ map (\(a, b) -> count1 s1 s2 a b [] 0) [(x, y) | x <- s'', y <- s''']
                        
{- https://discourse.haskell.org/t/folding-a-lcm-of-a-list/841/2 -}
lcmm :: Integral a => [a] -> a
lcmm [] = 1
lcmm (x:xs) = lcm x (lcmm xs)

count1 :: String -> [String] -> String -> String -> [String] -> Int -> Int
count1 str x acc1 acc2 acc3 i = let m = Map.fromList (helper x [])
                                    rez = f str m acc1
                                      in if (rez == acc2) then (i + 1) * (length str) else if (elem rez acc3) then (-1) else count1 str x rez acc2 ([rez] ++ acc3) (i + 1)
                                               

helper :: [String] -> [(String, (String, String))] -> [(String, (String, String))] 
helper [] acc             = acc
helper (x : xs) acc       = let x' = filter (\x -> (elem x (['A'..'Z'] ++ ['0'..'9']))) x
                                x1 = take 3 x'
                                x2 = take 3 (drop 3 x')
                                x3 = drop 6 x'
                                        in helper xs (acc ++ ([(x1, (x2, x3))]))
                                        
f :: String -> Map.Map String (String, String) -> String -> String
f [] _ acc = acc
f (x : xs) m acc = if (x == 'L') then f xs m (fst (m Map.! acc)) else f xs m (snd (m Map.! acc))

f' :: [String] -> [String] -> [String]
f' [] acc = acc
f' (x : xs) acc = if (x!!2 == 'A') then f' xs (acc ++ [(take 3) x]) else f' xs acc

f'' :: [String] -> [String] -> [String]
f'' [] acc = acc
f'' (x : xs) acc = if (x!!2 == 'Z') then f'' xs (acc ++ [(take 3) x]) else f'' xs acc