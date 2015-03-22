{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
module Main where

import BasePrelude hiding ((<|>),first)

-- For the REPL
import System.IO

-- For parsing/eval
import Control.Monad.State (modify,get)
import Control.Monad.Except (MonadError,runExceptT,throwError)

import Control.Lens ((^?),isn't)

import Data.Bifunctor (first)
import Text.ParserCombinators.Parsec hiding (spaces)

-- Required for managing state
import qualified Data.Map as Map

import LeespTypes
import LeespParser

main :: IO ()
main = do
  args <- getArgs
  if null args
    then runRepl
    else runOne args

-- REPL FUNS
flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> String
evalString env str = case readExpr str of
  Left err -> show err
  Right exp -> case runEval env (eval exp) of
    (Left err, s) -> show err
    (Right v, s) -> show v

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env = putStrLn . evalString env 

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  unless (pred result) $ action result >> until_ pred prompt action

-- This version of runOne expects a file input to read from.
runOne :: [String] -> IO ()
runOne args = evalAndPrint newEnv $ "(import " ++ head args ++ ")"
  where
    newEnv = bindVars primitiveBindings [("args", List . map String $ drop 1 args)]

readEvalPrintLoop :: Env -> IO ()
readEvalPrintLoop env = do
  s <- readPrompt "Leesp >> "
  unless (s == "quit") $ do
    let (r,e') = either ((,env) . Left) (runEval env . eval) $ readExpr s
    putStrLn $ case r of
     Left err -> mappend "*** Error ***" $ show err
     Right v -> showVal v
    readEvalPrintLoop e'

runRepl :: IO ()
runRepl = do
  displayReplBanner
  readEvalPrintLoop primitiveBindings

displayReplBanner :: IO ()
displayReplBanner =
  let bannerLines = ["Welcome to the Leesp REPL!",
                     "You can import leebs by using the (import \"path/to/your/leeb\")",
                     "To quit just type in 'quit'.",
                     "Multiline expressions are not yet supported, sorry. :("]
  in putStrLn `mapM_` bannerLines

-- END REPL FUNS

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "leesp" input of
  Left err -> throwError $ Parser err
  Right val -> return val

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseExpr

readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow (endBy parseExpr (comments <|> spaces))

apply :: LispVal -> [LispVal] -> Eval LispVal
apply (PrimitiveFunc fun) args = fun args
apply (Func parms vargs body closure) args =
  if intLen parms /= intLen args && isNothing vargs
  then throwError $ NumArgs (intLen parms) args
  else do ev' <- bindVarArgs vargs $ bindVars closure zippedArgs
          case evalBody ev' body of
            (Left err, badState) -> throwError err
            (Right val, goodState) -> return val
  where
    intLen = toInteger . length    
    remaining = drop (length parms) args
    zippedArgs = zip parms args
    bindVarArgs arg env = return $ case arg of
      Just argName -> bindVars env [(argName, List remaining)]
      Nothing -> env
    evalBody env = last . map (runEval env . eval)

eval :: LispVal -> Eval LispVal
eval val@(String _)               = return val
eval val@(Number _)               = return val
eval val@(Bool _)                 = return val
eval val@(Character _)            = return val
eval val@(Keyword _)              = return val
eval (Atom a) = do env <- get
                   case Map.lookup a env of
                     Nothing -> throwError $ UnboundVar "unbound var" a
                     Just val -> return val
-- Handled Quoting.
eval (List [Atom "quote", val])   = return val
-- Bind a variable -- is for mutable variables, leaving out for now! :)
eval (List [Atom "set!", Atom var, form]) = eval form >>= setVar var
-- Flow Control Functions
eval (List [Atom "if", pred, conseq, alt]) = evIfFun pred conseq alt
eval (List (Atom "cond" : items)) = evCondFun items
eval (List (Atom "case" : sel : choices)) = eval sel >>= evCaseFun choices
-- Define a variable
eval (List [Atom "define", Atom var, form]) = eval form >>= defineVar var
eval (List (Atom "define" : List (Atom var : parms) : body)) = makeNormalFunc parms body >>= defineVar var
eval (List (Atom "define" : DottedList (Atom var : parms) varargs : body)) = makeVarArgs varargs parms body >>= defineVar var
-- Anonymous function definitions
eval (List (Atom "lambda" : DottedList parms varargs : body)) = makeVarArgs varargs parms body
eval (List (Atom "lambda" : varargs@(Atom _) : body)) = makeVarArgs varargs [] body
eval (List (Atom "lambda" : List parms : body)) = makeNormalFunc parms body
eval (List (function : args)) = do
  func <- eval function
  argVals <- mapM eval args
  apply func argVals
-- Yer done gone f**ked up.
eval badForm = throwError $ BadSpecialForm "Unrecognised special form" badForm

makeFunc :: Maybe String -> [LispVal] -> [LispVal] -> Eval LispVal
makeFunc vargs parms body = do env <- get
                               return (Func (map showVal parms) vargs body env)

makeNormalFunc :: [LispVal] -> [LispVal] -> Eval LispVal
makeNormalFunc = makeFunc Nothing

makeVarArgs :: LispVal -> [LispVal] -> [LispVal] -> Eval LispVal
makeVarArgs = makeFunc . Just . showVal

defineFoo :: LispVal
defineFoo = List [ Atom "define"
                  , List [ Atom "foo"
                         , Atom "a"
                         , Atom "b"
                         ]
                  , List [ Atom "+"
                         , Atom "a"
                         , Atom "b"]
                  ]

appFoo :: LispVal
appFoo = List [Atom "foo", Number 3, Number 3]

evIfFun :: LispVal -> LispVal -> LispVal -> Eval LispVal
evIfFun pred conseq alt =
  do p <- eval pred
     case p of
       Bool True -> eval conseq
       Bool False -> eval alt
       _ -> throwError $ TypeMismatch "Non boolean result in 'if'" p

evCondFun :: [LispVal] -> Eval LispVal
evCondFun [] = throwError $ BadSpecialForm "Non-exhaustive patterns in" $ String "cond"
evCondFun [Atom "otherwise", conseq] = eval conseq
evCondFun (pred:conseq:rest) = do
  result <- eval pred
  case result of
    Bool True  -> eval conseq
    Bool False -> evCondFun rest
    otherwise  -> throwError $ TypeMismatch "boolean" pred
    
evCaseFun :: [LispVal] -> LispVal -> Eval LispVal
evCaseFun [] selector = throwError $ BadSpecialForm "Non-exhaustive patterns in " selector
evCaseFun (comparator:conseq:rest) selector = do
  choice <- if caseHasAtom comparator then return comparator else eval comparator
  equiv <- eqv [selector,choice]
  case equiv of
    Bool True -> eval conseq
    Bool False -> evCaseFun rest selector
  where
    caseHasAtom (Atom _) = True
    caseHasAtom _        = False

primitives :: [(String, [LispVal] -> Eval LispVal)]
primitives =
        -- Basic Maths Functions
        [ ("+", numericBinop (+)),
          ("-", numericBinop (-)),
          ("*", numericBinop (*)),
          ("/", numericBinop div),
          ("mod", numericBinop mod),
          ("quotient", numericBinop quot),
          ("remainder", numericBinop rem),
        -- Comparison Functions
          ("=", numBoolBinop (==)),
          ("<", numBoolBinop (<)),
          (">", numBoolBinop (>)),
          ("/=", numBoolBinop (/=)),
          (">=", numBoolBinop (>=)),
          ("<=", numBoolBinop (<=)),
          ("&&", boolBoolBinop (&&)),
          ("||", boolBoolBinop (||)),
          ("eq?", eqv),
          ("eqv?", eqv),
          ("equal?", equal),
        -- List Functions
          ("head", leespHead),
          ("rest", leespRest),
          ("cons", cons),
        -- String Functions
          ("string=?", strBoolBinop (==)),
          ("string?", strBoolBinop (>)),
          ("string<=?", strBoolBinop (<=)),
          ("string>=?", strBoolBinop (>=)),
          ("string", makeStringFromArgs),
          ("string-length", stringLength),
          ("string-ref", stringRefFn),
          ("make-string", makeStringN),
          ("string-insert!", stringinsertFn),
          ("substring", subStringFn)]

-- ioPrimitives :: [(String, [LispVal] -> Eval LispVal)]
-- ioPrimitives = [("apply", applyProc),
--                 ("open-input-file", makePort ReadMode),
--                 ("open-output-file", makePort WriteMode),
--                 ("close-input-port", closePort),
--                 ("close-output-port", closePort),
--                 ("read", readProc),
--                 ("write", writeProc),
--                 ("read-contents", readContents),
--                 ("read-all", readAll)]

subStringFn :: [LispVal] -> Eval LispVal
subStringFn [String s, Number start, Number end]
  | indicesValid = (return . String) $ genericSubList s
  | otherwise = throwError $ Default $ "substring: indices out of range for input: " ++ s
  where
    indicesValid = 0 <= start && start <= end && end <= genericLength s
    genericSubList = genericTake (end - start) . genericDrop start
-- Provide some at least slightly useful errors.
subStringFn [s, n, k] = throwError $ TypeMismatches "target (string) start (number) end (number)" [s, n, k]
subStringFn badArgList = throwError $ NumArgs 3 badArgList

integerCount :: [a] -> Int
integerCount = length

stringinsertFn [String s, Number k, Character c] =
  if integerCount s >= idx
    then (return . String) $ x ++ [c] ++ xs
    else throwError $ Default "string-insert!: index out of range!"
  where
    idx = fromInteger k
    (x,xs) = splitAt idx s
stringSetFn badArgList = throwError $ NumArgs 3 badArgList

stringRefFn :: [LispVal] -> Eval LispVal
stringRefFn [String s, Number n] = (return . Character) $ genericIndex s n
stringRefFn [String _, badArg]   = throwError $ TypeMismatch "number" badArg
stringRefFn [badArg, Number _]   = throwError $ TypeMismatch "string" badArg
stringRefFn badArgList           = throwError $ NumArgs 2 badArgList

buildString :: String -> [LispVal] -> String
buildString acc [] = acc
buildString acc (Character c : rest) = buildString (acc ++ [c]) rest

makeStringFromArgs :: [LispVal] -> Eval LispVal
makeStringFromArgs val@(Character _ :_) = return . String $ buildString [] val
makeStringFromArgs badArg               = throwError $ NumArgs 2 badArg

stringLength :: [LispVal] -> Eval LispVal
stringLength [String val] = (return . Number) $ genericLength val
stringLength [badArg]     = throwError $ TypeMismatch "string" badArg
stringLength badArgList   = throwError $ NumArgs 1 badArgList

makeStringN :: [LispVal] -> Eval LispVal
makeStringN [Number n, Character c] = return . String . genericTake n $ repeat c
makeStringN [Number n, _]           = return . String $ genericTake n ['a'..]
makeStringN [bad, _]                = throwError $ TypeMismatch "number [char]" bad
makeStringN badArgList              = throwError $ NumArgs 1 badArgList

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> Eval LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op parms = fin $ app parms
  where
    err = throwError $ TypeMismatch "number" (head parms) 
    fin = maybe err (return . Number)
    app = fmap (foldr1 op) . traverse (^? _Number)

boolBinop :: (LispVal -> Maybe a) -> (a -> a -> Bool) -> [LispVal] -> Eval LispVal
boolBinop p op [l,r] =
  case op <$> left <*> right of
    Just b -> return (Bool b)
    _ -> throwError $ TypeMismatch ("between " ++ show l ++ " and ") r
  where
    left = p l
    right = p r
boolBinop _ _ ns = throwError $ NumArgs 2 ns

numBoolBinop = boolBinop (^?_Number)
strBoolBinop = boolBinop (^?_String)
boolBoolBinop = boolBinop (^?_Bool)

leespHead :: [LispVal] -> Eval LispVal
leespHead [List (x:xs)]         = return x
leespHead [DottedList (x:xs) _] = return x
leespHead [badArg]              = throwError $ TypeMismatch "pair" badArg
leespHead badArgList            = throwError $ NumArgs 1 badArgList

leespRest :: [LispVal] -> Eval LispVal
leespRest [DottedList (_ : xs) x] = return $ DottedList xs x
leespRest [DottedList [xs] x]     = return x
leespRest [List (x:xs)]           = return $ List xs
leespRest [badArg]                = throwError $ TypeMismatch "pair" badArg
leespRest badArgList              = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> Eval LispVal
cons [x1, List []]            = return $ List [x1]
cons [x, List xs]             = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2]                 = return $ DottedList [x1] x2
cons badArgList               = throwError $ NumArgs 2 badArgList

eqv :: (Applicative m, Monad m, MonadError LispError m) => [LispVal] -> m LispVal
eqv [Bool arg1, Bool arg2]             = return $ Bool $ arg1 == arg2
eqv [Number arg1, Number arg2]         = return $ Bool $ arg1 == arg2
eqv [String arg1, String arg2]         = return $ Bool $ arg1 == arg2
eqv [Atom arg1, Atom arg2]             = return $ Bool $ arg1 == arg2
eqv [DottedList xs x, DottedList ys y] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [List arg1, List arg2]             = do
  a <- goo rst
  eqv $ Bool ln : a
  where
    ln = length arg1 == length arg2
    rst = zip arg1 arg2
    goo = traverse (\(a,b) -> eqv [a,b])
    
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

unpackEquals :: Eq a => Bool -> [LispVal] -> (LispVal -> Maybe a) -> Eval LispVal
unpackEquals match [a,b] p = if match
                             then eqv [a, b]
                             else case (==) <$> p a <*> p b of
                               Just a -> return $ Bool a
                               Nothing -> throwError $ TypeMismatch "comparison with" b

equal :: [LispVal] -> Eval LispVal
equal xs@[Number a , b] = unpackEquals (isn't _Number b) xs (^?_Number)
equal xs@[String a , b] = unpackEquals (isn't _String b) xs (^?_String)
equal xs@[Bool a , b] = unpackEquals (isn't _Bool b) xs (^?_Bool)
equal badArgList = throwError $ NumArgs 2 badArgList

-- START STATE MGMT
emptyEnv :: Env
emptyEnv = Map.empty

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = liftM extractValue (runExceptT (trapError action))

setVar :: String -> LispVal -> Eval LispVal
setVar var value = do env <- get
                      case Map.lookup var env of
                        Nothing -> defineVar var value
                        Just _ -> throwError $ BoundVar "Variable already bound" value

defineVar :: String -> LispVal -> Eval LispVal
defineVar var value = do
  modify $ Map.insert var value
  return value

bindVars :: Env -> [(String, LispVal)] -> Env
bindVars envM bindings = Map.union envM $ Map.fromList bindings

primitiveBindings :: Env
primitiveBindings = bindVars emptyEnv prims
  where
    mkFunc constr (var,fn) = (var, constr fn)
    mapMkFun t = map (mkFunc t)
    prims = mapMkFun PrimitiveFunc primitives
    --prims = (mapMkFun (IOFunc) ioPrimitives) ++ (mapMkFun (PrimitiveFunc) primitives)

-- START IO PRIMITIVES
-- applyProc :: [LispVal] -> Eval LispVal
-- applyProc [func, List args] = apply func args
-- applyProc (func : args) = apply func args

-- makePort :: IOMode -> [LispVal] -> Eval LispVal
-- makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

-- closePort ∷ [LispVal] -> Eval LispVal
-- closePort [Port port] = liftIO (hClose port) >> return (Bool True)

-- readProc :: [LispVal] -> Eval LispVal
-- readProc [] = readProc [Port stdin]
-- readProc [Port port] = liftIO (hGetLine port) >>= liftThrows . readExpr

-- writeProc :: [LispVal] -> Eval LispVal
-- writeProc [obj] = writeProc [obj, Port stdout]
-- writeProc [obj, Port port] = liftIO $ hPrint port obj >> return (Bool True)

-- readContents :: [LispVal] -> Eval LispVal
-- readContents [String filename] = liftM String $ liftIO $ readFile filename

-- load :: String -> IO [LispVal]
-- load filename = liftIO (readFile filename) >>= liftThrows . readExprList

-- readAll :: [LispVal] -> Eval LispVal
-- readAll [String filename] = liftM List $ load filename
-- END IO PRIMITIVES
