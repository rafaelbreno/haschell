module Parser (
    LispVal(..),
    parseExpr,
    readExpr,
    trapError,
    extractValue,
    eval
) where

import Text.ParserCombinators.Parsec as P hiding (spaces)
import Data.Char (digitToInt)
import Numeric (readOct, readHex)
import Data.Functor as Functor
import Data.Ratio ((%))
import Data.Complex (Complex((:+)))
import Text.Parsec (spaces)
import Control.Monad.Except as E

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

instance Show LispVal where show = showVal

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = unwordsList contents
showVal (DottedList h t) = "(" ++ unwordsList h ++ " . " ++ showVal t ++ ")"
showVal _ = error "not implemented yet"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal


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
parseExprList :: P.Parser [LispVal]
parseExprList = sepBy parseExpr spaces

parseTail :: P.Parser (Maybe LispVal)
parseTail = 
  (P.char '.' >> spaces >> parseExpr >>= \expr -> return (Just expr)) 
  <|> return Nothing

-- Here `h` mean head, and `t` means tail,
-- named them like that to avoid overwriting 
-- `head` and `tail` built-in funcs
makeList :: [LispVal] -> Maybe LispVal -> LispVal
makeList h Nothing = List h
makeList h (Just t) = DottedList h t

parseListOrDottedList :: P.Parser LispVal
parseListOrDottedList = do 
  h <- parseExprList 
  makeList h <$> parseTail 

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
         x <- parseListOrDottedList
         _ <- char ')'
         return x

symbol :: P.Parser Char
symbol = P.oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> ThrowsError LispVal
readExpr input = case P.parse parseExpr "lisp" input of
  Left err -> E.throwError $ Parser err
  Right val -> return val

-- evaluator
eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = E.throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (E.throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [
  ("+", numericBinop (+)),
  ("-", numericBinop (-)),
  ("+", numericBinop (+)),
  ("/", numericBinop div),
  ("mod", numericBinop mod),
  ("quotient", numericBinop quot),
  ("remainder", numericBinop rem),
  ("symbol?", isSymbol),
  ("symbol->string", symbolToString),
  ("string->symbol", stringToSymbol),
  ("string?", isString),
  ("number?", isNumber),
  ("boolean?", isBool),
  ("char?", isCharacter),
  ("list?", isList)
  ]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop _            [] = E.throwError $ NumArgs 2 []
numericBinop _ singleVal@[_] = E.throwError $ NumArgs 2 singleVal
numericBinop op params       = mapM unpackNum params <&> (Number . foldl1 op) 
-- │        mapM unpackNum params <&> (Number . foldl1 op) 

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in 
                           if null parsed 
                              then E.throwError $ TypeMismatch "number" $ String n
                              else return $ fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = E.throwError $ TypeMismatch "number" notNum

isSymbol :: [LispVal] -> ThrowsError LispVal
isSymbol [Atom _] = return $ Bool True
isSymbol _ = return $ Bool False

symbolToString :: [LispVal] -> ThrowsError LispVal
symbolToString [Atom s] = return $ String s
symbolToString _        = error "symbol->string expects a single symbol argument"

stringToSymbol :: [LispVal] -> ThrowsError LispVal
stringToSymbol [String s] = return $ Atom s
stringToSymbol _        = error "string->symbol expects a single string argument"


isBool :: [LispVal] -> ThrowsError LispVal
isBool [Bool _] = return $ Bool True
isBool _ = return $ Bool False

isString :: [LispVal] -> ThrowsError LispVal
isString [String _] = return $ Bool True
isString _ = return $ Bool False

isNumber :: [LispVal] -> ThrowsError LispVal
isNumber [Number _] = return $ Bool True
isNumber _ = return $ Bool False

isCharacter :: [LispVal] -> ThrowsError LispVal
isCharacter [Character _] = return $ Bool True
isCharacter _ = return $ Bool False

isList :: [LispVal] -> ThrowsError LispVal
isList [List _] = return $ Bool True
isList _ = return $ Bool False


data LispError = NumArgs        Integer [LispVal]
               | TypeMismatch   String LispVal
               | Parser         P.ParseError
               | BadSpecialForm String LispVal
               | NotFunction    String String
               | UnboundVar     String String
               | Default        String
               | NotImplemented String


instance Show LispError where show = showError

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected 
                                       ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected 
                                       ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at: " ++ show parseErr
showError (NotImplemented message)      = message
showError _ = error "not implemented yet"

type ThrowsError = Either LispError

trapError :: ThrowsError String -> ThrowsError String
trapError action = E.catchError action ( return . show )

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
