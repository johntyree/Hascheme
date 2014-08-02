

module Main where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Complex
import Data.Maybe
import Data.Ratio
import Numeric
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces, many, (<|>), optional)


main :: IO ()
main = do
    args <- getArgs
    print args
    mapM_ putStrLn $ fmap readExpr args


symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"


spaces :: Parser ()
spaces = skipMany1 space


data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Float Double
             | Rational (Ratio Integer)
             | Complex (Complex Double)
             | String String
             | Bool Bool
             | Character String
    deriving Show


readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val


parseBool :: Parser LispVal
parseBool = Bool . (== 't') <$> (char '#' *> oneOf "ft")


parseString :: Parser LispVal
parseString = String <$> between quote quote escChars
  where
    quote = char '"'
    escChars = many (escapedOrNot "nrt\\\"")


escapedOrNot :: String -> Parser Char
escapedOrNot escChars = readChar <$> (escaped <|> noneOf "\"")
  where
    escaped = char '\\' *> oneOf escChars
    readChar c = read $ "'\\" ++ [c] ++ "'"


parseCharacter :: Parser LispVal
parseCharacter = (Character . readChar) <$> (prefix *> many nonWhitespace)
    where prefix = try $ char '#' *> char '\\'
          nonWhitespace = satisfy (not . isSpace)
          readChar [c] = return . read $ "'\\" ++ [c] ++ "'"
          readChar s = s


parseAtom :: Parser LispVal
parseAtom = Atom <$> atom
  where
    first = letter <|> symbol
    rest = many (letter <|> digit <|> symbol)
    atom = (:) <$> first <*> rest


parseDecimalNumber :: Parser LispVal
parseDecimalNumber = Number . read <$> (optional (string "#d") *> many1 digit)

parseBinaryNumber :: Parser LispVal
parseBinaryNumber = parseRadixNumber "01" readBin
  where
    readBin = readInt 2 (`elem` "01") (read . return)

parseOctalNumber :: Parser LispVal
parseOctalNumber = parseRadixNumber "01234567" readOct

parseHexNumber :: Parser LispVal
parseHexNumber = parseRadixNumber "0123456789abcdefABCDEF" readHex

parseRadixNumber :: String -> ReadS Integer -> Parser LispVal
parseRadixNumber chars decimalize = try $ prefix *> fmap numberfy digits
  where
    prefix = char '#' *> char 'o'
    numberfy = Number . fst . head . decimalize
    digits = many1 (oneOf chars)

parseNumber :: Parser LispVal
parseNumber = try (parseOctalNumber
               <|> parseHexNumber
               <|> parseBinaryNumber
               <|> parseDecimalNumber)

parseFloat :: Parser LispVal
parseFloat = fmap (Float . fst . head . readFloat) parseFloatString

parseFloatString :: Parser String
parseFloatString = try $ do
    beginning <- many digit <* char '.'
    end <- many digit
    guard $ (not . null) (beginning ++ end)
    -- We can slap extra 0's in there to avoid ".###" and "###.".
    return $ "0" ++ beginning ++ "." ++ end ++ "0"

parseRational :: Parser LispVal
parseRational = try $ numberfy <$> int <* over <*> int
  where
    int = many1 digit
    over = ws <* char '/' <* ws
    ws = skipMany space
    numberfy num denom = Rational (read num % read denom)

parseComplex :: Parser LispVal
parseComplex = try $ numberfy <$> real <*> imag
  where
    real = read . fromMaybe "0" <$> (optionMaybe . try $ numFollowedBy '+')
    imag = read <$> numFollowedBy 'i'
    numberfy r i = Complex (r :+ i)
    numFollowedBy c = (parseFloatString <|> many1 digit) <* char c


parseExpr :: Parser LispVal
parseExpr  = parseRational
         <|> parseComplex
         <|> parseFloat
         <|> parseNumber
         <|> parseCharacter
         <|> parseAtom
         <|> parseString
         <|> parseBool
