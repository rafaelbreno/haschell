module Parser (
    LispVal(..),
    parseExpr,
    readExpr
) where

import Text.ParserCombinators.Parsec as P hiding (spaces)
import Data.Char (digitToInt)
import Numeric (readOct, readHex)
import Data.Functor as Functor
import Data.Ratio ((%))
import Data.Complex (Complex((:+)))
import Text.Parsec (spaces)

data LispVal = Atom             String 
             | Bool             Bool
             | Character        Char
             | Complex          (Complex Double)
             | DottedList       [LispVal] LispVal -- This is called "improper list", stores a list of all elements but the last one, and the stores the last element separately.
             | Float            Double 
             | List             [LispVal]
             | Number           Integer
             | Rational         Rational
             | String           String
             | Quasinote        LispVal
             | Unquote          LispVal
             | UnquoteSplicing  LispVal
             | Vector           [LispVal]

-- Atom
parseAtom :: P.Parser LispVal
parseAtom = do 
  first <- P.letter P.<|> symbol
  rest  <- many (P.letter P.<|> P.digit P.<|> symbol)
  let atom = first:rest
  return $ case atom of 
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom

-- Bool

-- Character
parseCharacter :: P.Parser LispVal
parseCharacter = do 
  _ <- P.try $ P.string "#\\"
  value <- parseNamedChar <|> parseAnyChar
  return $ Character value

parseNamedChar :: P.Parser Char
parseNamedChar = do 
  P.choice (map P.try 
    [ P.string "space" >> return ' '
    , P.string "newline" >> return '\n'
    ])

parseAnyChar :: P.Parser Char 
parseAnyChar = do 
  x <- P.anyChar 
  notFollowedByAlphaNum 
  return x

notFollowedByAlphaNum :: P.Parser () 
notFollowedByAlphaNum = do 
  nextChar <- P.lookAhead $ P.optionMaybe P.alphaNum 
  case nextChar of 
    Just _ -> P.unexpected "character"
    Nothing -> return ()

-- DottedList

-- List
parseList :: P.Parser LispVal
parseList = List <$> sepBy parseExpr spaces

parseDottedList :: P.Parser LispVal
parseDottedList = do 
  h <- P.endBy parseExpr spaces -- head
  t <- P.char '.' >> spaces >> parseExpr -- tail
  return $ DottedList h t

parseQuoted :: P.Parser LispVal
parseQuoted = do 
  _ <- char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]


-- Number

parseNumber :: P.Parser LispVal
parseNumber = do 
  prefix <- P.optionMaybe (P.char '#')
  case prefix of 
    Nothing -> parseDecimal <|> parseRational <|> parseComplex <|> parseFloat
    Just _ -> parsePrefixedNumber

parseDecimal :: P.Parser LispVal
parseDecimal = do 
  digits <- P.many1 P.digit
  return (Number ( read digits ))

parseFloat :: P.Parser LispVal
parseFloat = do
  whole <- P.many1 P.digit
  _ <- P.char '.'
  fractional <- P.many1 P.digit
  return $ Float (read (whole ++ "." ++ fractional))

parseRational :: P.Parser LispVal
parseRational = do
  numerator <- P.many1 P.digit
  _ <- P.char '/'
  denominator <- P.many1 P.digit
  return $ Rational (read numerator % read denominator)

parseComplex :: P.Parser LispVal
parseComplex = do 
  realPart <- P.try parseFloat <|> parseDecimal
  _ <- P.char '+'
  imagPart <- P.try parseFloat <|> parseDecimal
  _ <- P.char '-'
  return $ case (realPart, imagPart) of 
    (Float r, Float i) -> Complex (r :+ i)
    (Float r, Number i) -> Complex (r :+ fromIntegral i)
    (Number r, Float i) -> Complex (fromIntegral r :+ i)
    (Number r, Number i) -> Complex (fromIntegral r :+ fromIntegral i)
    (_, _) -> error "not implement"


parsePrefixedNumber :: P.Parser LispVal
parsePrefixedNumber = do 
  base <- P.oneOf "bodx"
  digits <- P.many1 ( P.digit <|> P.oneOf "abcdefABDEF")
  return $ Number (convertToInt base digits)

convertToInt :: Char -> String -> Integer
convertToInt 'b' = bin2dec
convertToInt 'o' = fst . head . readOct
convertToInt 'd' = read
convertToInt 'x' = fst . head . readHex
convertToInt _ = error "not supported"

bin2dec :: String -> Integer
bin2dec = foldl (\acc x -> acc * 2 + toInteger (digitToInt x)) 0

-- String

parseString :: P.Parser LispVal
parseString = do 
  _ <- P.char '"'
  x <- many parseStringChar
  _ <- P.char '"'
  return $ String x

parseStringChar :: P.Parser Char
parseStringChar = do 
  P.char '\\' *> (
    P.char '"' Functor.$> '"' 
    <|> P.char 'n' Functor.$> '\n'
    <|> P.char 'r' Functor.$> '\r'
    <|> P.char 't' Functor.$> '\t'
    <|> P.char '\\' Functor.$> '\\'
    ) 
  <|> noneOf "\\\""

-- Quasinote
parseQuasinote :: P.Parser LispVal 
parseQuasinote = do 
  _ <- P.char '`'
  x <- parseExpr
  return $ List [Atom "quasinote", x]

parseUnquote :: P.Parser LispVal
parseUnquote = do 
  _ <- P.char ','
  x <- parseExpr
  return $ List [Atom "unquote", x]

parseUnquoteSplicing :: P.Parser LispVal
parseUnquoteSplicing = do 
  _ <- P.try $ P.string ",@"
  x <- parseExpr
  return $ List [Atom "unquote-splicing", x]

-- Vector
parseVector :: P.Parser LispVal
parseVector = do 
  _ <- P.string "#("
  elems <- P.sepBy parseExpr spaces
  _ <- P.char '('
  return $ Vector elems


-- expr

parseExpr :: P.Parser LispVal
parseExpr = parseAtom 
  <|> parseString
  <|> parseNumber
  <|> parseCharacter
  <|> parseQuoted
  <|> parseQuasinote
  <|> parseUnquote
  <|> parseUnquoteSplicing
  <|> parseVector
  <|> do 
         _ <- char '('
         x <- P.try parseList <|> parseDottedList
         _ <- char ')'
         return x

symbol :: P.Parser Char
symbol = P.oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case P.parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right _ -> "Found a value"
