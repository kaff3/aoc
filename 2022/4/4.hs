import Data.List(elemIndex)

getRange :: String -> (Int, Int)
getRange s = 
    case elemIndex '-' s of 
        Just i -> 
            let (xs, '-':ys) = splitAt i s in 
                (read xs, read ys)
        Nothing -> error "invalid input"

-- 4.1
isSubRange :: String -> Int
isSubRange s =
    case elemIndex ',' s of
        Just i -> 
            let (xs, ',':ys) = splitAt i s 
                (r1s, r1e) = getRange xs
                (r2s, r2e) = getRange ys in
                    if (r1s <= r2s && r1e >= r2e) ||
                    (r2s <= r1s && r2e >= r1e) then 1
                    else 0
        Nothing -> error "invalid input"

numSubRanges :: [String] -> Int 
numSubRanges = foldl (\acc s -> acc + isSubRange s) 0

-- 4.2
isOverlapRange :: String -> Int
isOverlapRange s =
    case elemIndex ',' s of
        Just i -> 
            let (xs, ',':ys) = splitAt i s 
                (r1s, r1e) = getRange xs
                (r2s, r2e) = getRange ys in
                    if (r1s >= r2s && r1s <= r2e) ||
                       (r1e >= r2s && r1e <= r2e) ||
                       (r2s >= r1s && r2s <= r1e) ||
                       (r2e >= r1s && r2e <= r1e) then 1
                    else 0
        Nothing -> error "invalid input"

numOverlapRanges :: [String] -> Int 
numOverlapRanges = foldl (\acc s -> acc + isOverlapRange s) 0

main :: IO()
main = do
    contents <- getContents 
    print $ numSubRanges $ lines contents
    print $ numOverlapRanges $ lines contents
