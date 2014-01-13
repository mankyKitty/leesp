module LeespParser where

import LeespTypes
import Control.Monad (liftM)
import Numeric (readOct, readHex)
import Text.ParserCombinators.Parsec hiding (spaces)

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/<=?>@^_~"

spaces :: Parser ()
spaces = skipMany1 space

--comments :: Parser ()
--comments = do
--  try $ string ";;"
--  skipMany1 anyChar

parseCharacter :: Parser LispVal
parseCharacter = do try $ string "#\\"
                    value <- try (string "newline" <|> string "space")
                             <|> do { x <- anyChar; notFollowedBy alphaNum ; return [x] }
                    return $ Character $ case value of
                      "space" -> ' '
                      "newline" -> '\n'
                      otherwise -> head value

escapeChars :: Parser Char
escapeChars = do char '\\'
                 x <- oneOf "\\\"nrt"
                 return $ case x of
                   '\\' -> x
                   '"' -> x
                   'n' -> '\n'
                   'r' -> '\r'
                   't' -> '\t'

parseBool :: Parser LispVal
parseBool = do char '#'
               (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

parseKeyword :: Parser LispVal
parseKeyword = do char ':'
                  x <- many1 (letter <|> digit <|> symbol)
                  return $ Keyword $ ':' : x

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many (escapeChars <|> noneOf "\"\\")
                 char '"'
                 return $ String x

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = first : rest
               return $ case atom of
                          "#t" -> Bool True
                          "#f" -> Bool False
                          otherwise -> Atom atom

parseNumber :: Parser LispVal
parseNumber = parseDigital1 <|> parseDigital2 <|> parseHex <|> parseOct <|> parseBin

parseDigital1 :: Parser LispVal
parseDigital1 = liftM (Number . read) (many1 digit)

parseDigital2 :: Parser LispVal
parseDigital2 = do try $ string "#d"
                   x <- many1 digit
                   (return . Number . read) x

parseHex :: Parser LispVal
parseHex = do try $ string "#x"
              x <- many1 hexDigit
              return $ Number (hex2dig x)

parseOct :: Parser LispVal
parseOct = do try $ string "#o"
              x <- many1 octDigit
              return $ Number (oct2dig x)

parseBin :: Parser LispVal
parseBin = do try $ string "#b"
              x <- many1 (oneOf "10")
              return $ Number (bin2dig x)

oct2dig x = fst $ readOct x !! 0

hex2dig x = fst $ readHex x !! 0

bin2dig = bin2dig' 0

bin2dig' digint "" = digint
bin2dig' digint (x:xs) = let old = 2 * digint + (if x == '0' then 0 else 1) in
                         bin2dig' old xs

parseExpr :: Parser LispVal
parseExpr = parseAtom
            <|> parseString
            <|> parseKeyword
            <|> try parseNumber
            <|> try parseBool
            <|> try parseCharacter
            <|> parseQuoted
            <|> do char '('
                   x <- (try parseList) <|> parseDottedList
                   char ')'
                   return x

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces -- (comments <|> spaces)

parseDottedList :: Parser LispVal
parseDottedList = do head <- endBy parseExpr spaces
                     tail <- char '.' >> spaces >> parseExpr
                     return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do char '\''
                 x <- parseExpr
                 return $ List [Atom "quote", x]
