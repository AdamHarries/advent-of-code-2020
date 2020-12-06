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



export 
search : List Int -> Int -> Bool 
search [] _ = False
search (y :: xs) x = case x == y of 
    True => True
    _ => search xs x

export
findPair : List Int -> Maybe (Int, Int)
findPair [] = Nothing
findPair (x :: xs) = let diff = 2020 - x in 
    -- We don't need to search backwards - we can always search forwards because if a previous 
    -- element was our pair, we would have found it. 
    if search xs diff then 
        Just (x, diff)
    else 
        findPair xs