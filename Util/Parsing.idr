module Util.Parsing

import System.File
import Data.Either
import Data.Strings

export 
readAsIntegers : String -> IO (Maybe (List Int))
readAsIntegers fname = do 
    contents <- System.File.readFile fname >>= (pure . getRight) 
    pure $ (contents >>= (\s => sequence $ map parseInteger (lines s)))
    