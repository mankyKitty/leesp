{-# LANGUAGE NoImplicitPrelude #-}

module LeespTypes where

import Prelude hiding (head,tail)

import Data.IORef
import qualified Data.Map as Map

import System.IO (Handle)

import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State

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
             | Func {params :: [String], vararg :: Maybe String, fnBody :: [LispVal], closure :: Env}
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Port Handle

data LispError = NumArgs Integer [LispVal]
         | TypeMismatch String LispVal
         | TypeMismatches String [LispVal]
         | Parser ParseError
         | BadSpecialForm String LispVal
         | NotFunction String String
         | UnboundVar String String
         | BoundVar String LispVal
         | Default String

type ThrowsError = Either LispError

type Env = IORef [(String, IORef LispVal)]

type Env' = Map.Map String LispVal

type Eval a = ReaderT Env' (ErrorT LispError (StateT Env' Identity)) a

runEval :: Env' -> Env' -> Eval a -> (Either LispError a,Env')
runEval env st ev = runIdentity (runStateT (runErrorT (runReaderT ev env)) st)

type IOThrowsError = ErrorT LispError IO

instance Show LispError where show = showError
instance Show LispVal where show = showVal

instance Error LispError where
  noMsg = Default "An error has occurred"
  strMsg = Default

trapError :: (MonadError e m, Show e) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
extractValue (Left _) = error "Extract Value shouldn't be here"

unWordsList :: [LispVal] -> String
unWordsList = unwords . map showVal

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ":" ++ varname
showError (BadSpecialForm message form) = message ++ ":" ++ show form
showError (NotFunction message func)    = message ++ ":" ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected ++ " args: found values " ++ unWordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
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
showVal (PrimitiveFunc _)      = "<PrimitiveFunc>"
showVal (Func {params = args, vararg = varargs}) =
    "(lambda (" ++ unwords (map show args) ++ (case varargs of
        Nothing -> ""
        Just arg -> " . " ++ arg) ++ ") ...)"
showVal (Port _)               = "<IO port>"
showVal (IOFunc _)             = "<IO primitive>"
