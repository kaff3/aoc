import Control.Monad.State
import Data.Tree

folderSizes :: String -> State Tree -> Int -> Int
folderSizes ("cd ":xs) f s 
    | xs == ".." = return s
    | xs == "/"  = return s
    | otherwise  = return s
folderSizes ("ls ":xs) f s = return s
folderSizes ("ls ":xs) f s = return s


main :: IO()
main = do
    contents <- getContents 
    print "great success"