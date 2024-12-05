import Data.Char (ord)
import Data.Map (fromList, member)
import Data.List(find)

getPriority :: Char -> Int
getPriority c = 
    let val = ord c in
        if val >= 97 then val - 96
        else val - 38

-- 3.1
findDuplicate :: String -> Char
findDuplicate backpack = 
    let (c1, c2) = splitAt (length backpack `div` 2) backpack
        c2map = fromList $ map (\v -> (v, 0)) c2 in
     case find (\x -> member x c2map) c1 of
        Just x -> x
        Nothing -> error "invalid input"
        
backpackPriority :: String -> Int
backpackPriority backpack = getPriority $ findDuplicate backpack             

prioritiesSum :: [String] -> Int
prioritiesSum = foldl (\acc b -> acc + backpackPriority b) 0

-- 3.2
findBadgdeItem :: String -> String -> String -> Char
findBadgdeItem b1 b2 b3 = 
     let b2map = fromList $ map (\v -> (v, 0)) b2
         b3map = fromList $ map (\v -> (v, 0)) b3 in
     case find (\x -> member x b2map && member x b3map) b1 of
        Just x -> x
        Nothing -> error "invalid input"


badgeItemPriority :: String -> String -> String -> Int
badgeItemPriority b1 b2 b3 = getPriority $ findBadgdeItem b1 b2 b3

prioritiesGroupSum :: [String] -> Int ->Int
prioritiesGroupSum [] acc = acc
prioritiesGroupSum (b1:b2:b3:bs) acc = 
    prioritiesGroupSum bs (acc + badgeItemPriority b1 b2 b3)
prioritiesGroupSum _ _ = error "not perfect groups of 3"


-- read input
main :: IO ()
main = do
    contents <- getContents
    print $ prioritiesSum $ lines contents
    print $ prioritiesGroupSum (lines contents) 0