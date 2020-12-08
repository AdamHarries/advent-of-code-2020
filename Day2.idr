module Day2 

import Data.Vect
import Data.Strings
import Data.Bool.Xor
import Text.Token
import Text.Lexer
import Text.Lexer.Core
import public Text.Parser.Core
import public Text.Parser
import Util.Parsing

countIs : (Eq a, Foldable f) => a -> f a -> Integer 
countIs c s = foldl (\acc, e => if e == c then acc + 1 else acc) 0 s

record PasswordEntry where
   constructor Entry
   constraint : (Integer, Integer)
   character : Char
   password : String

hasValidCount : PasswordEntry -> Bool 
hasValidCount e = let instances = countIs e.character (unpack e.password) in 
   (instances >= (fst e.constraint)) && (instances <= (snd e.constraint))

hasValidPositions : PasswordEntry -> Bool 
hasValidPositions e = let 
   cs : String = singleton e.character
   ixl : Int = cast ((fst e.constraint) - 1)
   ixr : Int = cast ((snd e.constraint) - 1)
   l = strSubstr ixl 1 e.password
   r = strSubstr ixr 1 e.password in
      (l == cs) `xor` (r == cs) 

export
Show PasswordEntry where 
   show e = "Entry: '" ++ 
      (show $ fst e.constraint) ++ "-" ++ 
      (show $ snd e.constraint) ++ " " ++
      (singleton e.character) ++ ": " ++
      e.password ++ "'"

public export 
data ExpressionToken = Whitespace
   | Constraint Integer 
   | RangeOperator
   | PasswordLetter Char 
   | Password String
   
export 
Show ExpressionToken where
   show Whitespace = " "
   show (Constraint i) = "cstr " ++ (show i)
   show RangeOperator = "-"
   show (PasswordLetter l) = "pwl: " ++ (show l)
   show (Password s) = "pw: " ++ s
  
expressionTokens : TokenMap ExpressionToken
expressionTokens = [
   (some (is ' '), \x => Whitespace), 
   (digits, \x => Constraint (cast x)),
   (is '-', \x => RangeOperator), 
   (alpha <+> is ':', \x => PasswordLetter (case (strUncons x) of 
         Nothing => '@'
         Just (c, _) => c)
   ), 
   (some alpha, \x => Password x)
]

public export
Rule : Type -> Type
Rule ty = Grammar (TokenData ExpressionToken) True ty

export
rangeLiteral : Rule ()
rangeLiteral
  = terminal "Expected range character" (\x => case tok x of
                  RangeOperator => Just ()
                  _ => Nothing)

export
intLiteral : Rule Integer
intLiteral
  = terminal "Expected integer constraint" (\x => case tok x of
                  Constraint i => Just i
                  _ => Nothing)

export 
whitespaceLiteral : Rule ()
whitespaceLiteral = terminal "Expected separating whitespace" (\x => case tok x of
   Whitespace => Just ()
   _ => Nothing)
                  

export 
constrCharLiteral : Rule Char 
constrCharLiteral = terminal "Expected character" (\x => case tok x of
   PasswordLetter l => Just l
   _ => Nothing)

export 
passwordLiteral : Rule String
passwordLiteral = terminal "Expected password" (\x => case tok x of
   Password s => Just s
   _ => Nothing)

export
pwentry : Rule PasswordEntry
pwentry = do 
   l <- intLiteral
   rangeLiteral 
   r <- intLiteral
   whitespaceLiteral
   c <- constrCharLiteral
   whitespaceLiteral
   p <- passwordLiteral
   pure (Entry (l, r) c p)


parseEntry : String -> Either (ParseError (TokenData ExpressionToken)) (PasswordEntry, List (TokenData ExpressionToken))
parseEntry s = parse pwentry (fst (lex expressionTokens s))

export
Show (ParseError tok) where 
   show (Error s l) = s 

   
main: IO ()
main = do 
   -- Read in the list of files 
   passwords <- readAsStrings "data/day2.txt"
   let entries = do 
      e <- (sequence $ map parseEntry passwords)
      pure $ map fst e
   putStrLn $ show entries
   case entries of 
      Right es => do 
         putStrLn $ "Has valid counts: " ++ (show $ countIs True $ map hasValidCount es)
         putStrLn $ "Has valid positions: " ++ (show $ countIs True $ map hasValidPositions es)
      _ => putStrLn "Failed to parse one or more entries"
   
