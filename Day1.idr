module Day1
import Data.Vect
import Util.Parsing

mutual 
main: IO ()
main = do 
    -- Read in the list of files 
    expenses <- readAsIntegers "data/day1.txt"
    let res = (expenses >>= findPair)
    case res of 
        Nothing => putStrLn "Failed to find a pair (parse error, or search error)" 
        Just (l, r) => do 
            putStrLn $ "Found pair (" ++ (show l) ++ "," ++ (show r) ++ "), multiple " ++ (show (l * r))
    let tripres = expenses >>= findTriple
    case tripres of 
        Nothing => putStrLn "Failed to find a triple (parse error, or search error)" 
        Just (a, b, c) => do 
            putStrLn $ "Found triple (" ++ (show a) ++ "," ++ (show b) ++ "," ++ (show c) ++ "), multiple " ++ (show (a *b*c))

export
findDiff : Int -> List Int -> Maybe (Int, Int)
findDiff sum [] = Nothing
findDiff sum (x :: xs) = let diff = sum - x in 
    -- We don't need to search backwards - we can always search forwards because if a previous 
    -- element was our pair, we would have found it. 
    if diff `elem` xs then 
        Just (x, diff)
    else 
        findDiff sum xs
            
export
findPair : List Int -> Maybe (Int, Int)
findPair xs = findDiff 2020 xs

export
findTriple : List Int -> Maybe (Int, Int, Int)
findTriple [] = Nothing 
findTriple (x :: xs) = let diff = 2020 - x in 
    case findDiff diff xs of 
        Nothing => findTriple xs 
        Just (b, c) => Just (x, b, c)
