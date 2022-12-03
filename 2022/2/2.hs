getValue :: Char -> Int
getValue c 
    | c == 'A' || c == 'X' = 0 -- rock
    | c == 'B' || c == 'Y' = 1 -- paper
    | c == 'C' || c == 'Z' = 2 -- scissor
    | otherwise = error "invalid input"

-- 2.1
getValues :: String -> (Int, Int)
getValues (p1:' ':p2:_) = 
    (getValue p1, getValue p2)
getValues _ = error "invalid input"

calcScore :: (Int, Int) -> Int
calcScore (p1, p2)
    | p2 == p1           = 3 
    | p2 == (p1 + 1) `mod` 3 = 6
    | otherwise          = 0

makePlay :: String -> Int
makePlay play =
    let (p1, p2) = getValues play 
    in (p2 + 1) + calcScore (p1, p2)

totalScore :: [String] -> Int
totalScore = foldl (\acc s -> acc + makePlay s) 0

-- 2.2
getValues2 :: String -> (Int, Char)
getValues2 (p1:' ':p2:_) = 
    (getValue p1, p2)
getValues2 _ = error "invalid input"

calcScore2 :: (Int, Char) -> Int
calcScore2 (p1, 'Y') = 3 + p1               -- draw
calcScore2 (p1, 'X') = 0 + (p1 - 1) `mod` 3 -- lose
calcScore2 (p1, 'Z') = 6 + (p1 + 1) `mod` 3 -- win
calcScore2 _ = error "invalid input"

makePlay2 :: String -> Int
makePlay2 play =
    let (p1, p2) = getValues2 play 
    in  calcScore2 (p1, p2) + 1

modifiedTotalScore :: [String] -> Int 
modifiedTotalScore = foldl (\acc s -> acc + makePlay2 s) 0


-- read input
main :: IO ()
main = do
    contents <- getContents
    print $ totalScore (lines contents)
    print $ modifiedTotalScore (lines contents)