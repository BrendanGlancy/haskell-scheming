module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad

-- Return values
data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

-- Parses string outputs LispVal
parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many (noneOf "\"")
                char '"'
                return $ String x

parseAtom :: Parser LispVal
parseAtom = do
             first <- letter <|> symbol
             rest <- many (letter <|> digit <|> symbol)
             let atom = first:rest
             return $ case atom of 
                        "#t" -> Bool True
                        "#f" -> Bool False
                        _    -> Atom atom

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseQuoted
         <|> do char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x


parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

-- parse parenthesized list
parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

-- telling Haskell how to print out a string representation of the various possible LispVals
showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"

-- matches one of the constructors of LispVal, and the right-hand side tells what to do for a value of that constructor
showVal (List contents) = "(" ++ unwordsLists contents ++ ")"
showVal (DottedList head tail) ="(" ++ unwordsLists head ++ " . " ++ showVal tail ++ ")"

-- unwordsLists, similar to built in unwords function, glues together a list of words with spaces
-- since were dealing with LispVals instead of words, function first converts LispVals into their string representation then applies unwords to it
unwordsLists :: [LispVal] -> String
unwordsLists = unwords . map showVal

-- show standard Haskell function that lets you convert any type that's an instance of the class Show into a string. 
-- to do the same with LispVal, we make it a member of the show class defining its show method as showVal
instance Show LispVal where show = showVal

-- Evaluating numbers, strings, booleans, and quoted list
eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val

-- paser function 
readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> String $ "No match: " ++ show err
    Right val -> val 

-- main function 
main :: IO ()
main = getArgs >>= print . eval . readExpr . head

