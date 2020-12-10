module Util.Common

import System.File
import Data.Either
import Data.Strings
import Data.Vect
import Decidable.Equality

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
readToString : String -> IO (String)
readToString fname = do 
    contents <- System.File.readFile fname >>= (pure . getRight)
    pure $ case contents of 
        Just c => c 
        Nothing => ""

export 
countIs : (Eq a, Foldable f) => a -> f a -> Integer 
countIs c s = foldl (\acc, e => if e == c then acc + 1 else acc) 0 s

-- Does a list of `a`'s contain a specific `a` 
export
contains : Eq a => List a -> a -> Bool 
contains [] _ = False 
contains (x :: xs) v = if x == v then True else (contains xs v)

-- Are the contents of one list contained in a second list, up to permutation?
export
containedIn : Eq a => List a -> List a -> Bool 
containedIn [] ys = True -- reached the end without returning false, we must be okay
containedIn (x :: xs) ys = if ys `contains` x then containedIn xs ys else False

export
permEq' : Eq a => Vect n a -> Vect n a -> Bool
permEq' Nil Nil = True 
permEq' (x::as) (y::bs) = if x == y then (permEq' as bs) else False 
permEq' _ _ = False
    
export
permEq : Eq a => {n : Nat} -> {m : Nat} -> Vect n a -> Vect m a -> Bool 
permEq xs ys = case decEq n m of 
    Yes p => permEq' xs (rewrite p in ys)
    No _ => False

export 
permEqL : Eq a => List a -> List a -> Bool 
permEqL xs ys = (fromList xs) `permEq` (fromList ys)
