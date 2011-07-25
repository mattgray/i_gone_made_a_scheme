module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

-- main

main :: IO ()
main = do 
    args <- getArgs
    putStrLn $ readExpr $ head args

-- parser

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse (spaces >> symbol) "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found Value"

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

-- language

data LispVal    = Atom String
                | List [LispVal]
                | DottedList [LispVal] LispVal
                | Number Integer
                | String String
                | Bool Bool


