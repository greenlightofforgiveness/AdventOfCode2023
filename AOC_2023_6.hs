module AOC_2023_6 where
import Data.List.Split (splitOn)
import Data.Char (isDigit)
import System.IO (readFile)
   
main1 = do  
    contents <- readFile "input.txt"  
    putStrLn $ analyze1 contents

main2 = do  
    contents <- readFile "input.txt"  
    putStrLn $ analyze2 contents
    
analyze1 :: String -> String
analyze1 s = let f x = map (\n -> read n :: Int) $ filter (/= "") $ splitOn (" ") $ tail (dropWhile (/= ':') x)
                 s' = splitOn "\n" s
                        in show (count1 (f (s' !! 0), f (s' !! 1)) 1)
                        
count1 :: ([Int], [Int]) -> Int -> Int
count1 ([], []) acc         = acc
count1 (t : ts, r : rs) acc = let       t' = read (show t) :: Double
                                        r' = read (show r) :: Double
                                        d = sqrt (t' ^ 2 - 4 * r')
                                        (a1, b1) = properFraction ((t' - d) / 2)
                                        (a2, b2) = properFraction ((t' + d) / 2)
                                        x1' = (a1 + 1)
                                        x2' = if (b2 /= 0.0) then a2 else (a2 - 1)
                                        acc' = ((min t x2') - (max 1 x1') + 1)
						in if (acc' /= 0) then count1 (ts, rs) (acc * acc') else count1 (ts, rs) acc                        
                        
analyze2 :: String -> String
analyze2 s = let f x = (\n -> read n :: Int) $ filter (isDigit) $ tail (dropWhile (/= ':') x)
                 s' = splitOn "\n" s
                        in show (count2 (f (s' !! 0), f (s' !! 1)))                        

count2 :: (Int, Int) -> Int
count2 (t, r)   = let   t' = read (show t) :: Double
                        r' = read (show r) :: Double
                        d = sqrt (t' ^ 2 - 4 * r')
                        (a1, b1) = properFraction ((t' - d) / 2)
                        (a2, b2) = properFraction ((t' + d) / 2)
                        x1' = (a1 + 1)
                        x2' = if (b2 /= 0.0) then a2 else (a2 - 1)
                                in ((min t x2') - (max 1 x1') + 1)