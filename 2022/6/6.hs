import Data.List(elemIndex)

getStreamStart :: String -> [Char] -> Int -> Int -> Int
getStreamStart (x:xs) q i st =
    if length q == st then i
    else 
        case elemIndex x q of
            Nothing -> getStreamStart xs (q++[x]) (i+1) st
            Just index -> 
                let q' = drop (index+1) q in
                    getStreamStart xs (q'++[x]) (i+1) st
getStreamStart _ _ _ _ = error "invalid input"

main :: IO()
main = do
    contents <- getContents
    print $ getStreamStart contents [] 0 4
    print $ getStreamStart contents [] 0 14