module AOC_2023_2 where
import System.IO     
import Data.List.Split (splitOn)
import Data.Char (isDigit)
import System.IO (readFile)
import Data.List (isInfixOf)
    
main1 = do  
    contents <- readFile "input.txt"  
    putStrLn $ analyze1 contents

analyze1 :: String -> String
analyze1 s = show (count1 (splitOn "\n" s) 0)

main2 = do  
    contents <- readFile "input.txt"  
    putStrLn $ analyze2 contents

analyze2 :: String -> String
analyze2 s = show (count2 (splitOn "\n" s) 0)

count_cubes :: [String] -> (Int, Int, Int) -> (Int, Int, Int)
count_cubes [] (r, g, b)        = (r, g, b)
count_cubes (x:xs) (r, g, b)    = let (r', g', b') = count_cubes' x
                                      in count_cubes xs (max r r', max g g', max b b')

count_cubes' :: String -> (Int, Int, Int)
count_cubes' [] = (0, 0, 0)
count_cubes' x  =  let  x'              = splitOn ", " x -- example: ["3 blue","4 red"]
                        [r', g', b']    = foldl (\x y -> [x!!0 + y!!0, x!!1 + y!!1, x!!2+ y!!2]) [0, 0, 0] (map (\k -> if (isInfixOf "red" k) then [read (takeWhile (/=' ') k) :: Int, 0, 0] else if (isInfixOf "green" k) then [0, read (takeWhile (/=' ') k) :: Int, 0] else [0, 0, read (takeWhile (/=' ') k) :: Int]) x') 
                                        in (r', g', b')

count1 :: [String] -> Int -> Int
count1 [] acc          = acc
count1 (x : xs) acc    = let d = read (filter isDigit (takeWhile (/= ':') x )) :: Int
                             x' = splitOn "; " (drop 2 $ dropWhile (/=':') x)
                             (r, g, b) = count_cubes x' (0,0,0) in if (r <= 12) && (g <= 13) && (b <= 14) then count1 xs (acc + d) else count1 xs acc

count2 :: [String] -> Int -> Int
count2 [] acc          = acc
count2 (x : xs) acc    = let d = read (filter isDigit (takeWhile (/= ':') x )) :: Int
                             x' = splitOn "; " (drop 2 $ dropWhile (/=':') x)
                             (r, g, b) = count_cubes x' (0,0,0) in count2 xs acc + r * g * b