module LeespTypes where

import Control.Monad.Error
import Text.ParserCombinators.Parsec (ParseError)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Character Char
             | Keyword String
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func {params :: [String], vararg :: (Maybe String), body :: [LispVal], closure :: Env}

data LispError = NumArgs Integer [LispVal]
         | TypeMismatch String LispVal
         | TypeMismatches String [LispVal]
         | Parser ParseError
         | BadSpecialForm String LispVal
         | NotFunction String String
         | UnboundVar String String
         | Default String

type ThrowsError = Either LispError

instance Show LispError where show = showError
instance Show LispVal where show = showVal

instance Error LispError where
  noMsg = Default "An error has occurred"
  strMsg = Default

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

unWordsList :: [LispVal] -> String
unWordsList = unwords . map showVal

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ":" ++ varname
showError (BadSpecialForm message form) = message ++ ":" ++ show form
showError (NotFunction message func)    = message ++ ":" ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected ++ " args: found values " ++ unWordsList found
showError (TypeMismatch expected found) = "Invalid trype: expected " ++ expected ++ ", found " ++ show found
showError (TypeMismatches expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ unWordsList found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr
showError (Default message)             = message

showVal :: LispVal -> String
showVal (Character c)          = show c
showVal (String contents)      = "\"" ++ contents ++ "\""
showVal (Atom name)            = name
showVal (Number contents)      = show contents
showVal (Bool True)            = "#t"
showVal (Bool False)           = "#f"
showVal (List contents)        = "(" ++ unWordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unWordsList head ++ " . " ++ showVal tail ++ ")"
showVal (Keyword kword)        = kword
