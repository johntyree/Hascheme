

module Main where

import Control.Monad
import Data.Char
import Numeric
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)


main :: IO ()
main = do
    args <- getArgs
    print args
    mapM_ putStrLn $ fmap readExpr args


symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"


spaces :: Parser ()
spaces = skipMany1 space


data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Character String
    deriving Show

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val


parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many (escapedOrNot "nrt\\\"")
    char '"'
    return $ String x


escapedOrNot :: String -> Parser Char
escapedOrNot escChars = escaped <|> (noneOf "\"")
    where escaped = char '\\' >> oneOf escChars >>= readChar
          readChar c = return . read $ "'\\" ++ [c] ++ "'"


parseCharacter :: Parser LispVal
parseCharacter = do
    try $ char '#' >> char '\\'
    rest <- many $ satisfy (not . isSpace)
    return $ Character rest


parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _    -> Atom atom


-- parseFloat :: Parser LispVal
-- parseFloat = do
    -- [(raw, suffix)] <- readFloat
    -- if

parseDecimalNumber :: Parser LispVal
parseDecimalNumber = liftM (Number . read) $ many1 digit

parseOctalNumber :: Parser LispVal
parseOctalNumber = do
    try $ char '#' >> char 'o'
    n <- many1 (oneOf "01234567")
    let [(val, _)] = readOct n
    return (Number val)

parseBinaryNumber :: Parser LispVal
parseBinaryNumber = do
    try $ char '#' >> char 'b'
    n <- many1 (oneOf "01")
    let [(val, _)] = readBin n
    return (Number val)
  where
    readBin = readInt 2 (`elem` "01") (read . return)

parseHexNumber :: Parser LispVal
parseHexNumber = do
    try $ char '#' >> char 'x'
    n <- many1 (oneOf "0123456789abcdefABCDEF")
    let [(val, _)] = readHex n
    return (Number val)

parseNumber :: Parser LispVal
parseNumber = parseOctalNumber
          <|> parseHexNumber
          <|> parseBinaryNumber
          <|> parseDecimalNumber

parseNumber' :: Parser LispVal
parseNumber' = do
    digits <- many1 digit
    return . Number . read $ digits

parseNumber'' :: Parser LispVal
parseNumber'' = many1 digit >>= return . Number. read


parseExpr :: Parser LispVal
parseExpr  = parseNumber
         <|> parseCharacter
         <|> parseString
         <|> parseAtom
