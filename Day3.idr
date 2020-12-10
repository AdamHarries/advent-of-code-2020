module Day3

import Data.Vect
import Data.List
import Data.Strings
import Data.Nat
import Data.Bool.Xor
import Text.Token
import Text.Lexer
import Text.Lexer.Core
import public Text.Parser.Core
import public Text.Parser
import Util.Parsing

public export 
data MapSpace = Empty | Tree 

export
Show MapSpace where 
    show Empty = "."
    show Tree = "#"

export 
Eq MapSpace where 
    Empty == Empty = True 
    Tree  == Tree  = True 
    _     == _     = False 
    a     /= b     = not (a == b)

spaceTokens : TokenMap MapSpace
spaceTokens = [
    (is '.', \x => Empty), 
    (is '#', \x => Tree)
]

lexRow : String -> List MapSpace
lexRow s = map tok $ fst (lex spaceTokens s)

len : Nat 
len = 31

ix : {lenMo: _} -> Nat -> {auto prf: len = S lenMo} -> (Fin len)
ix n = rewrite prf in restrict lenMo (cast n)
    
indices : {lenMo: _} -> {len: Nat} -> Nat -> Nat -> {auto prf: len = S lenMo} -> List (Fin len)
indices n m = [ix (i*m) | i <- [0..n]]

everyOther : List a -> List a 
everyOther [] = [] 
everyOther (a :: []) = [a]
everyOther (a :: b :: xs) = a :: (everyOther xs)

traverse : List (Fin len, Vect len MapSpace) -> List MapSpace
traverse m = map (\(ix, row) => index ix row ) m

mapType : Type
mapType = List (Vect len MapSpace)

findSolution : {lenMo: _} -> {len: Nat} -> List (Vect len MapSpace) -> (Nat, Bool) -> {auto prf: len = S lenMo} -> Integer 
findSolution m (routeR, routeD)  = do 
    let rows = length m
    let ixs : List (Fin len) = indices {len=len} rows routeR
    let m = if routeD then (everyOther m) else m 
    let r = traverse $ zip ixs m
    countIs Tree r

main : IO ()
main = do 
    strMap <- readAsStrings "data/day3.txt"
    let lstMap = map lexRow strMap
    let rows = length lstMap
    let ixs : List (Fin len) = indices {len=len} rows 3
    let vecMap : Maybe (List (Vect len MapSpace)) = sequence $ map (toVect len) lstMap
    case vecMap of 
        Just m => do 
            let routes : List (Nat, Bool) = [(1, False), (3, False), (5, False), (7, False), (1, True)]
            let res1 = map (\r  => findSolution m r) routes
            putStrLn $ show $ res1
            putStrLn $ show $ foldl (*) 1 res1
        Nothing => putStrLn "Failed to parse map"