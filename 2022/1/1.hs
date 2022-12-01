import Control.Monad  
import Data.Char  

mostCalories :: [String] -> Int -> Int -> Int
mostCalories [] acc most = most
mostCalories ("":xs) acc most =
    if acc > most then mostCalories xs 0 acc
    else mostCalories xs 0 most
mostCalories (x:xs) acc most =
    mostCalories xs (acc + read x) most

main :: IO ()
main = do
    contents <- getContents
    let allLines = lines contents
    print $ mostCalories allLines 0 0