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
symbol = oneOf "!$%&|*/:<=>?@^_~"

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

parseFloat :: Parser LispVal
parseFloat = parsePrefixedFloat <|> parsePlainFloat


parseComplex' :: Parser LispVal
parseComplex' = do
    real <- parseNumber
    imaginary <- optionMaybe $ do {i <-parseNumber; char 'i'; return i}
    return $ case imaginary of
        Nothing -> real
        (Just imaginary') -> Complex $ ((value real) :+ (value imaginary'))
        where
            value :: LispVal -> Double
            value (Number x) = fromIntegral x
            value (Float x) = x
            value _ = error "not a numeric LispVal"


parsePlainFloat = liftM (Float . read) $ try $ float

parsePrefixedFloat = liftM (Float . read) $ try $ string "#d" >> float

float = do
            x <- many1 digit
            char '.'
            y <- many1 digit
            return $ x ++ "." ++ y

parseNumber = do
    sign <- option '+' $ oneOf "+-"
    intPart <- intPart'
    fracPart <- optionMaybe fracPart'
    return $ case fracPart of
        Nothing -> Number $ (sign' sign) intPart
        Just x -> Float $ (sign' sign) ((fromIntegral intPart) + x) 
        where 
            sign' :: Num a => Char -> (a -> a)
            sign' '+' = id
            sign' '-' = negate
            sign _ = error "not a sign"

fracPart' :: Parser Double
fracPart' = liftM (read . ((++) "0."))  $ char '.' >> many1 digit

intPart' :: Parser Integer
intPart' = liftM read $ many1 digit

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

-- things beginning in #
-- true, false, radix prefixed number, characters
parseHash :: Parser LispVal
parseHash = do
    char '#'
    x <- oneOf "tf\\oxbd"
    rest <- option "" (characterName <|> character)
    return $ case x of 
        't'     -> Bool True
        'f'     -> Bool False
        '\\'    -> getCharacter rest

characterName = string "space" <|> string "newline"

character = do
    rest' <- anyChar
    notFollowedBy alphaNum
    return [rest']

getCharacter "space" = Character ' '
getCharacter "newline" = Character '\n'
getCharacter [c] = Character c
getCharacter _ = error "parsing character literal - multiple characters after #\\" 


-- expression parser

parseExpr :: Parser LispVal
parseExpr = parseAtom 
    <|> parseString
    <|> parseComplex' 
    <|> parseHash
    -- <|> try parseBool
    -- <|> try parseChar
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
            -- atoms
            (Atom "anAtom", "anAtom")
            , (Atom "an!Atom", "an!Atom")
            -- strings
            , (String "a string", "\"a string\"")
            , (String "a \"escaped string", "\"a \\\"escaped string\"")
            , (String "a \t \n \r string", "\"a \\t \\n \\r string\"")
            -- numbers
            , (Number 1, "1")
            , (Number 1, "+1")
            , (Number (-1), "-1")
            , (Number 12345, "12345")
            , (Number 12345, "#d12345")
            , (Number 8, "#o10")
            , (Number 10, "#o12")
            , (Number 15, "#xf")
            , (Number 17, "#x11")
            , (Number 3, "#b11")
            , (Number 1, "1")
            , (Float 1.1111, "1.1111")
            , (Float 1.1111, "#d1.1111")
            , (Complex (3 :+ 2), "3+2i")
--            , (Complex (3 :+ (-2)), "3-2i")
            , (Complex (3.4 :+ 2.1), "3.4+2.1i")
            , (Complex (3 :+ 2.1), "3+2.1i")
            -- boolean
            , (Bool True, "#t")
            , (Bool False, "#f")
            -- character
            , (Character 'a', "#\\a")
            , (Character ' ', "#\\space")
            , (Character '\n', "#\\newline")
            -- list
            , (List [], "()")
            , (List [Atom "a", Atom "b"], "(a b)")
            , (List [Atom "a", String "b"], "(a \"b\")")
            , (List [Atom "a", List [Atom "b", Atom "c"]], "(a (b c))")
            -- dotted list
            , (DottedList [Atom "dotted"] (Atom "list"), "(dotted . list)")
            --quoted list
            , (List [Atom "quote", Atom "a"], "'a")
            , (List [Atom "quote", List [Atom "a",Atom "b"]], "'(a b)")
            --multi list
            , (List [Atom "a", List [Atom "quote",List [Atom "quoted", DottedList [Atom "dotted"] (String "list")]], Atom "test", Number 400], "(a '(quoted (dotted . \"list\")) test 400)")
            
            ]

thingsThatShouldntParse = [
            ("unterminated string", "\"blah blah")
            , ("symbol not allowed in atom", "(at.om a)")
            , ("atom begining in digit", "(8s dd)")
            , ("dodgy hash literal thingy", "#w")
            , ("mismatched parens", "(a")
            , ("comma in list", "(a , b)")
            , ("unmatched parens in quoted list", "('(a b")
            ]





runParserTests = runTestTT $ test $ shouldParseTests ++ shouldntParseTests where
            shouldParseTests = map makeTest thingsThatShouldParse where makeTest (expected, input) = TestLabel input $ TestCase $ shouldParse expected input
            shouldntParseTests = map makeTest thingsThatShouldntParse where makeTest (desc, input) = TestCase $ shouldntParse desc input 

shouldParse :: LispVal -> String -> Assertion
shouldParse expected input = assert' expected (readExpr' input) where
            assert' expected (Right val) = assertEqual "" expected val
            assert' expected (Left err) =  assertString ("Expected " ++ (show expected))

shouldntParse :: String -> String -> Assertion
shouldntParse desc input = assert' desc (readExpr' input) input where
            assert' desc (Right val) input = assertString (desc ++ " " ++ input ++ " produced " ++ (show val))
            assert' _ (Left err) _ = assertBool "" True
