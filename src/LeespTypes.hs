{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module LeespTypes where

import BasePrelude hiding (head,tail)
import qualified Data.Map as Map

import Control.Monad.Identity (Identity,runIdentity)
import Control.Monad.Except (ExceptT,MonadError,catchError,runExceptT)
import Control.Monad.State (StateT,runStateT)

import Control.Lens (makePrisms)

import Text.ParserCombinators.Parsec (ParseError)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Character Char
             | Keyword String
             | PrimitiveFunc ([LispVal] -> Eval LispVal)
             | Func { params :: [String]
                    , vararg :: Maybe String
                    , fnBody :: [LispVal]
                    , closure :: Env
                    }
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

type Env = Map.Map String LispVal

type Eval = ExceptT LispError (StateT Env Identity)

type IOThrowsError = ExceptT LispError IO

-- It's here because of template haskell enforcing the order
-- of definitions like a chump.
makePrisms ''LispVal

instance Show LispError where show = showError
instance Show LispVal where show = showVal

runEval :: Env -> Eval a -> (Either LispError a, Env)
runEval env st = runIdentity (runStateT (runExceptT st) env)

trapError :: (MonadError e m, Show e) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue = either (error "Oh dear..") id

unWordsList :: [LispVal] -> String
unWordsList = unwords . map showVal

showError :: LispError -> String
showError (BoundVar message varname)    = message ++ ":" ++ show varname
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
