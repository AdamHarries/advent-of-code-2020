module Test

import Data.Vect
import Day1
 
assert : (Eq a, Show a) => a -> a -> IO ()
assert gold res = case gold == res of 
    True => putStrLn $ "Test Passed (" ++ (show gold) ++ " == " ++ (show res) ++ ")"
    False => do 
        putStrLn "ERROR, values do not match!"
        putStrLn $ (show gold) ++ " =/= " ++ (show res)

mutual 

main : IO () 
main = do 
    day1


-- Day 1 tests 

-- Overall test harness.  
day1 : IO ()
day1 = do 
    putStrLn "Running Day 1 tests"
    findWhereExists
    findWhereNotExists
    findAPair

findWhereExists : IO () 
findWhereExists = do
    putStrLn "Test: findWhereExists"
    let haystack = fromList [25, 4, 53, 60, 2, 43, 222, 198, 10, 3]
    let needle : Int= 4
    let found = Day1.search haystack needle
    assert True found

findWhereNotExists : IO () 
findWhereNotExists = do
    putStrLn "Test: findWhereNotExists"
    let haystack = fromList [25, 4, 53, 60, 2, 43, 222, 198, 10, 3]
    let needle : Int = 409
    let found = Day1.search haystack needle
    assert False found


findAPair : IO () 
findAPair = do 
    let haystack = fromList [25, 4, 53, 60, 2, 43, 222, 198, 10, 3, 2016]
    let res = findPair haystack
    assert (Just (4, 2016)) res
