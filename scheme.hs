module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Numeric (readOct, readHex)
-- main

main :: IO ()
main = do 
    args <- getArgs
    putStrLn $ readExpr $ head args

-- parser

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found Value: " ++ show val

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
parseNumber =
    parsePlainFloat
    <|> parsePrefixedFloat
    <|> parsePlainInteger
    <|> parsePrefixedInteger
    <|> parseBinary
    <|> parseOctal
    <|> parseHex 

parsePlainFloat = liftM (Float . read) $ try $ parseFloat

parsePrefixedFloat = liftM (Float . read) $ try $ string "#d" >> parseFloat

parseFloat = do
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

-- expression

parseExpr :: Parser LispVal
parseExpr = parseAtom 
    <|> parseString
    <|> try parseNumber
    <|> try parseBool
    <|> try parseChar

-- language

data LispVal    = Atom String
                | List [LispVal]
                | DottedList [LispVal] LispVal
                | Number Integer
                | Float Double
                | String String
                | Bool Bool
                | Character Char
                deriving Show
