module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Numeric (readOct, readHex)
import Data.Complex
import Test.HUnit
-- main

main :: IO ()
main = do 
    args <- getArgs
    putStrLn $ readExpr $ head args

-- parser

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case readExpr' input of
    Left err -> "No match: " ++ show err
    Right val -> "Found Value: " ++ show val

readExpr' :: String -> Either ParseError LispVal 
readExpr' = parse parseExpr "lisp"

spaces :: Parser ()
spaces = skipMany1 space

--lisp atom

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many $ letter <|> digit <|> symbol
    let atom = first:rest
    return $ Atom atom   

-- lisp string

parseString :: Parser LispVal
parseString = do 
    char '"'
    x <- many $ unescaped <|> escaped
    char '"'
    return $ String x
    where
        unescaped = noneOf "\\\""
        escaped =  do 
            char '\\'
            x <- oneOf "\\\"nrt"
            return $ case x of
                '\\' -> x
                '\"' -> x
                'n' -> '\n'
                'r' -> '\r'
                't' -> '\t'

-- lisp char

parseChar :: Parser LispVal
parseChar = liftM (Character . getCharacter) $ string "#\\" >> (characterName <|> character) 
    where
        characterName = (try $ string "space" <|> string "newline")
        
        character = do
            x' <- anyChar
            notFollowedBy alphaNum
            return [x']
        
        getCharacter "space" = ' '
        getCharacter "newline" = '\n'
        getCharacter (c:cs) = c

-- lisp bool
parseBool :: Parser LispVal
parseBool = do
    char '#'
    x <- oneOf "tf"
    return $ case x of
        't' -> Bool True
        'f' -> Bool False

-- lisp number
parseNumber :: Parser LispVal
parseNumber = parsePlainInteger
    <|> parsePrefixedInteger
    <|> parseBinary
    <|> parseOctal
    <|> parseHex 

parseFloat :: Parser LispVal
parseFloat = parsePrefixedFloat <|> parsePlainFloat

parseComplex = do
    x <- parsePlainNumber
    char '+'
    y <- parsePlainNumber
    char 'i'
    return $ Complex $ ((read x) :+ (read y))
    where
	parsePlainNumber = try float <|> many1 digit 

parsePlainFloat = liftM (Float . read) $ try $ float

parsePrefixedFloat = liftM (Float . read) $ try $ string "#d" >> float

float = do
            x <- many1 digit
            char '.'
            y <- many1 digit
            return $ x ++ "." ++ y

parsePlainInteger = liftM (Number . read) $ many1 digit

-- prefixed number notation combinator
prefixNumberParser :: String -> (String -> Integer) -> Parser Char -> Parser LispVal
prefixNumberParser prefix reader parser' = liftM (Number . reader) $ try $ string prefix >> many1 parser'

parsePrefixedInteger = prefixNumberParser "#d" read digit

parseBinary = prefixNumberParser "#b" readBinary (oneOf "01")

readBinary = foldl readBinary' 0 where 
    readBinary' acc '0' = acc * 2  
    readBinary' acc '1' = (acc * 2) + 1  

parseOctal = prefixNumberParser "#o" readOct' octDigit where readOct' = fst . head . readOct

parseHex = prefixNumberParser "#x" readHex' hexDigit where readHex' = fst . head . readHex

-- lisp list
parseList :: Parser LispVal
parseList = liftM List $ parseExpr `sepBy` spaces 

-- lisp dotted list
parseDottedList :: Parser LispVal
parseDottedList = (liftM2 DottedList) head tail where
	head = parseExpr `endBy` spaces 
	tail = char '.' >> spaces >> parseExpr

-- lisp quoted list
parseQuoted :: Parser LispVal
parseQuoted = liftM quote $ char '\'' >> parseExpr where quote x = List [Atom "quote", x]


-- expression parser

parseExpr :: Parser LispVal
parseExpr = parseAtom 
    <|> parseString
    <|> try parseComplex
    <|> try parseFloat 
    <|> try parseNumber
    <|> try parseBool
    <|> try parseChar
    <|> parseQuoted
    <|> do 
	char '('
	x <- try parseList <|> parseDottedList
	char ')'
	return x

-- language

data LispVal    = Atom String
                | List [LispVal]
                | DottedList [LispVal] LispVal
                | Number Integer
                | Float Double
                | Complex (Complex Double)
		| String String
                | Bool Bool
                | Character Char
		deriving (Show, Eq)

--tests

thingsThatShouldParse = [
            (Atom "anAtom", "anAtom")
            , (String "a string", "\"a string\"")
            ]

thingsThatShouldntParse = [
            ("unterminated string", "\"blah blah")
            ]





runParserTests = runTestTT $ test $ shouldParseTests ++ shouldntParseTests where
            shouldParseTests = map makeTest thingsThatShouldParse where makeTest (expected, input) = TestCase $ shouldParse expected input
            shouldntParseTests = map makeTest thingsThatShouldntParse where makeTest (desc, input) = TestCase $ shouldntParse desc input 

shouldParse :: LispVal -> String -> Assertion
shouldParse expected input = assert' expected (readExpr' input) where
            assert' expected (Right val) = assertEqual "" expected val
            assert' expected (Left err) =  assertString ("Expected " ++ (show expected))

shouldntParse :: String -> String -> Assertion
shouldntParse desc input = assert' desc (readExpr' input) input where
            assert' desc (Right val) input = assertString (desc ++ " " ++ input ++ " produced " ++ (show val))
            assert' _ (Left err) _ = assertBool "" True
