module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad

-- main

main :: IO ()
main = do 
    args <- getArgs
    putStrLn $ readExpr $ head args

-- parser

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found Value: " ++ show val

spaces :: Parser ()
spaces = skipMany1 space

--list atom

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many $ letter <|> digit <|> symbol
    let atom = first:rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _    -> Atom atom   

-- lisp string

parseString :: Parser LispVal
parseString = do 
    char '"'
    x <- many (noneOf "\"")
    char '"'
    return $ String x

-- lisp number
parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

-- expression

parseExpr :: Parser LispVal
parseExpr = parseAtom 
    <|> parseString
    <|> parseNumber

-- language

data LispVal    = Atom String
                | List [LispVal]
                | DottedList [LispVal] LispVal
                | Number Integer
                | String String
                | Bool Bool
                deriving Show
