import Control.Monad  
import Data.List  

-- 1.1
-- main
mostCalories' :: [String] -> Int -> Int -> Int
mostCalories' [] acc most = most
mostCalories' ("":xs) acc most =
    if acc > most then mostCalories' xs 0 acc
    else mostCalories' xs 0 most
mostCalories' (x:xs) acc most =
    mostCalories' xs (acc + read x) most

-- entry
mostCalories :: String -> Int
mostCalories s = 
    let allLines = lines s
    in mostCalories' allLines 0 0 


-- 1.2
-- main
topMostCalories' :: [String] -> Int -> [Int] -> [Int]
topMostCalories' [] acc most = most
topMostCalories' ("":xs) acc most =
    let i = findIndex (acc <=) most 
    in case i of 
        Just 0 -> topMostCalories' xs 0 most
        Just x -> let index = x - 1
                      pre   = take index (tail most)
                      post  = drop (index + 1) most
             in topMostCalories' xs 0 (pre ++ [acc] ++ post)
        Nothing -> topMostCalories' xs 0 (tail most ++ [acc])
topMostCalories' (x:xs) acc most =
    topMostCalories' xs (acc + read x) most

-- entry
topMostCaloriesTotal :: String -> Int -> Int
topMostCaloriesTotal s top = 
    let allLines = lines s
        tops     = replicate top 0 
        topMost  = topMostCalories' allLines 0 tops  
    in sum topMost 

-- read input
main :: IO ()
main = do
    contents <- getContents
    print $ mostCalories contents
    print $ topMostCaloriesTotal contents 3