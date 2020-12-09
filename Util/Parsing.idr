module Util.Parsing

import System.File
import Data.Either
import Data.Strings

export
readAsIntegers : String -> IO (Maybe (List Int))
readAsIntegers fname = do
    contents <- System.File.readFile fname >>= (pure . getRight)
    pure $ (contents >>= (\s => sequence $ map parseInteger (lines s)))

export
readAsStrings : String -> IO (List String)
readAsStrings fname = do
    contents <- System.File.readFile fname >>= (pure . getRight)
    pure $ case contents of
        Just c => lines c
        Nothing => []

export 
countIs : (Eq a, Foldable f) => a -> f a -> Integer 
countIs c s = foldl (\acc, e => if e == c then acc + 1 else acc) 0 s
