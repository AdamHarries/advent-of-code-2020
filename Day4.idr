module Day4
 
import Data.Strings
import Data.Nat
import Text.Token
import Text.Lexer
import Text.Lexer.Core
import Text.Quantity
import public Text.Parser.Core
import public Text.Parser
import Util.Common

-- Data model definitions
eKeys : List String
eKeys = ["byr","iyr","eyr","hgt","hcl","ecl","pid"]

oKeys : List String 
oKeys = ["cid"]

keys : List String
keys = eKeys ++ oKeys

record PassportEntry where 
    constructor MkPassportEntry
    key : String
    value : String 
 
implementation Show PassportEntry where 
    show e = (key e) ++ ":" ++ (value e)

Passport : Type 
Passport = List PassportEntry

validPOne : Passport -> Bool 
validPOne p = eKeys `containedIn` (map key p)

-- More complex validation 
validKV : PassportEntry -> Bool 
validKV (MkPassportEntry "byr" a) = case (length a, parseInteger a) of 
    (4, Just i) => (i >= 1920) && (i <= 2002) 
    _ => False 
validKV (MkPassportEntry "iyr" a) = case (length a, parseInteger a) of 
    (4, Just i) => (i >= 2010) && (i <= 2020) 
    _ => False 
validKV (MkPassportEntry "eyr" a) = case (length a, parseInteger a) of 
    (4, Just i) => (i >= 2020) && (i <= 2030) 
    _ => False 
validKV (MkPassportEntry "hgt" a) = case lexHeight a of 
    (Cm h :: []) => (h >= 150) && (h <= 193) 
    (In h :: []) => (h >= 59) && (h <= 76) 
    _ => False
    where 
        data Height = In Int | Cm Int 
        lexHeight : String -> List Height
        lexHeight s = map tok $ fst (lex [
            (digits <+> exact "in", \x => In (cast (fst $ break (== 'i') x))),
            (digits <+> exact "cm", \x => Cm (cast (fst $ break (== 'c') x)))
        ] s)
validKV (MkPassportEntry "hcl" a) = case (length a, lexHexColor a) of 
    (7, (h :: [])) => True
    _ => False
    where
        lexHexColor : String -> List String
        lexHexColor  s = map tok $ fst (lex [
            (is '#' <+> hexDigits, \x => x)
        ] s)
validKV (MkPassportEntry "ecl" a) = ["amb","blu","brn","gry","grn","hzl","oth"] `contains` a 
validKV (MkPassportEntry "pid" a) = case (length a, parseInteger {a=Int} a) of 
    (9, Just i) => True 
    _ => False 
validKV (MkPassportEntry "cid" a) = True
validKV _ = False

validPTwo : Passport -> Bool 
validPTwo p = foldl (\acc, e => acc && e) True $ map validKV p

-- Lexer definitions
data PassportToken = Key String | Value String | Whitespace | Separator

passportTokenMap : TokenMap PassportToken
passportTokenMap = [
    (choice $ map (\k => exact k <+> is ':') keys, \x => Key (strSubstr 0 3 x)), 
    ((newline <+> (reject newline)) <|> (some (is ' ')), \x => Whitespace), 
    (newline <+> newline, \x => Separator),
    (some (alphaNum <|> is '#'), \x => Value x)
]

-- Parser definitions
Rule : Type -> Type
Rule ty = Grammar (TokenData PassportToken) True ty

passportEntry : Rule PassportEntry
passportEntry = do 
    key <- terminal "Expected passport entry key" (\x => case tok x of
        Key i => Just i
        _ => Nothing)
    value <- terminal "Expected passport entry value" (\x => case tok x of
        Value i => Just i
        _ => Nothing)
    pure $ MkPassportEntry key value

wsLiteral : Rule ()
wsLiteral = terminal "Expected whitespace separator" (\x => case tok x of
    Whitespace => Just ()
    _ => Nothing)

separatorLiteral : Rule ()
separatorLiteral = terminal "Expected newline separator" (\x => case tok x of
    Separator => Just ()
    _ => Nothing)

passport : Rule Passport 
passport = do
    optional wsLiteral 
    pes <- sepBy1 wsLiteral passportEntry 
    optional wsLiteral 
    pure pes

passports : Rule (List Passport)
passports = sepBy1 separatorLiteral passport

parsePassports : String -> Either (ParseError (TokenData PassportToken)) ((List Passport), List (TokenData PassportToken))
parsePassports s = parse passports (fst (lex passportTokenMap s))

-- Entrypoint
main : IO ()
main = do 
    input <- readToString "data/day4.txt"
    let parse = parsePassports input
    case parse of
        Right es => do 
            let ps = fst es 
            putStrLn $ show $ length ps
            let validPOnePs = map validPOne ps 
            putStrLn $ "ValidPOne count: " ++ (show $ countIs True validPOnePs)
            let validPTwoPs = map (\p => (validPOne p) && (validPTwo p)) ps 
            putStrLn $ "ValidPTwo count: " ++ (show $ countIs True validPTwoPs)
        _ => putStrLn "we dun fud"
