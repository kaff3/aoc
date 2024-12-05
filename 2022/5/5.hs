import Data.List(elemIndex, transpose)

-- quick and dirty stack implementation
type Stack = [Char]

newStack :: Stack
newStack = []

pop :: Stack -> (Char, Stack)
pop s = (head s, tail s)

push :: Stack -> Char -> Stack
push s x = x : s

pushMult :: Stack -> [Char] -> Stack
pushMult s xs = xs ++ s

popMult :: Stack -> Int -> (Stack, Stack)
popMult s i = splitAt i s

peek :: Stack -> Char
peek [] = ' '
peek s =  head s

initializeStack :: String -> Stack -> Stack
initializeStack [] s = s 
initializeStack (' ':str) s = initializeStack str s
initializeStack (c:str) s = initializeStack str (push s c)

initializeStacks :: [String] -> [Stack]
initializeStacks = map (initializeStack newStack)

parseInput :: [String] -> ([String], [String])
parseInput inp = 
    case elemIndex "" inp of
        Just i ->
            let (stacks, moves) = splitAt i inp
                stacks' = transpose $ init stacks 
                stacks'' = 
                    map (\i -> filter (\c -> c /= ' ')(stacks' !! i)) 
                        [1,5..(length stacks')] 
                in (stacks'', tail moves)
        Nothing -> error "invalid input"

parseMove :: String -> (Int, Int, Int)
parseMove str = 
    let strLst = words str
        num = read $ strLst !! 1 -- move x from y to z
        frm = read $ strLst !! 3
        to  = read $ strLst !! 5 in
        (num, frm-1, to-1) 

-- 5.1
makeMove :: (Int, Int, Int) -> [Stack] -> [Stack]
makeMove (0, _, _) stacks = stacks
makeMove (num, frm, to) stacks = 
    let (c, frmStack) = pop $ stacks !! frm
        toStack = push (stacks !! to) c
        newStacks = 
            map (\i -> if i == to then toStack
                       else if i == frm then frmStack
                       else stacks !! i) [0..(length stacks -1)]
    in makeMove (num-1, frm, to) newStacks 

getTopCrates :: [Stack] -> [String] -> String
getTopCrates stacks [] = 
    foldl (\acc s -> acc ++ [peek s]) "" stacks
getTopCrates stacks (move:moves) = 
    let m = parseMove move
        stack' = makeMove m stacks
    in getTopCrates stack' moves

-- 5.2
makeMove' :: (Int, Int, Int) -> [Stack] -> [Stack]
makeMove' (0, _, _) stacks = stacks
makeMove' (num, frm, to) stacks = 
    let (cs, frmStack) = popMult (stacks !! frm) num
        toStack = pushMult (stacks !! to) cs
        newStacks = 
            map (\i -> if i == to then toStack
                       else if i == frm then frmStack
                       else stacks !! i) [0..(length stacks -1)]
    in newStacks 

getTopCrates' :: [Stack] -> [String] -> String
getTopCrates' stacks [] = 
    foldl (\acc s -> acc ++ [peek s]) "" stacks
getTopCrates' stacks (move:moves) = 
    let m = parseMove move
        stack' = makeMove' m stacks
    in getTopCrates' stack' moves


main :: IO()
main = do
    contents <- getContents 
    let (stacksSetup, moves) = parseInput $ lines contents
        stacks = initializeStacks stacksSetup 
        in do
           print $ getTopCrates stacks moves
           print $ getTopCrates' stacks moves