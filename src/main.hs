module Main where

-- For genericN functions
import qualified Data.List as L
-- For the REPL
import System.IO hiding (try)
-- For parsing/eval
import Control.Monad
import Control.Monad.Error
-- Required for general operations.
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
-- Required for managing state
import Data.IORef
import Data.Maybe (isJust, isNothing)

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

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ liftThrows (readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do result <- prompt
                               unless (pred result) $ action result >> until_ pred prompt action

-- This version of runOne expects a file input to read from.
runOne :: [String] -> IO ()
runOne args = do
  env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
  runIOThrows (liftM show (eval env (List [Atom "import", String (head args)]))) >>= hPutStrLn stderr

runRepl :: IO ()
runRepl = do
  displayReplBanner
  primitiveBindings >>= until_ (== "quit") (readPrompt "Leesp >> ") . evalAndPrint

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

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func parms varargs body closure) args =
  if num parms /= num args && isNothing varargs
    then throwError $ NumArgs (num parms) args
    else liftIO (bindVars closure $ zip parms args) >>= bindVarArgs varargs >>= evalBody
  where
    remainingArgs = drop (length parms) args
    num = toInteger . length
    evalBody env = liftM last $ mapM (eval env) body
    bindVarArgs arg env = case arg of
      Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
      Nothing -> return env

eval :: Env -> LispVal -> IOThrowsError LispVal
-- Display Evaluated Values.
eval env val@(String _)               = return val
eval env val@(Number _)               = return val
eval env val@(Bool _)                 = return val
eval env val@(Character _)            = return val
eval env val@(Keyword _)              = return val
eval env (Atom id)                    = getVar env id
-- Handled Quoting.
eval env (List [Atom "quote", val])   = return val
-- Load a file and eval the contents
eval env (List [Atom "import", String filename]) =
  load filename >>= liftM last . mapM (eval env)
-- Flow Control Functions.
eval env (List [Atom "if", pred, conseq, alt]) =
  myIfFun env pred conseq alt
eval env (List (Atom "cond" : items)) =
  myCondFun env items
eval env (List (Atom "case" : sel : choices)) =
  eval env sel >>= myCaseFun env choices
eval env (List [Atom "set!", Atom var, form]) =
  eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
  eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : parms) : body)) =
  makeNormalFunc env parms body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : parms) varargs : body)) =
  makeVarargs varargs env parms body >>= defineVar env var
eval env (List (Atom "lambda" : List parms : body)) =
  makeNormalFunc env parms body
eval env (List (Atom "lambda" : DottedList parms varargs : body)) =
  makeVarargs varargs env parms body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
  makeVarargs varargs env [] body
eval env (List (function : args)) = do
  func <- eval env function
  argVals <- mapM (eval env) args
  apply func argVals
-- Yer done gone f**ked up.
eval env badForm =
  throwError $ BadSpecialForm "Unrecognised special form" badForm

myCaseFun :: Env -> [LispVal] -> LispVal -> IOThrowsError LispVal
myCaseFun _ [] selector = liftThrows . throwError $ BadSpecialForm "Non-exhaustive patterns in " selector
myCaseFun env (comparator:conseq:rest) selector = do
  choice <- if caseHasAtom comparator
            then return comparator
            else eval env comparator
  comparison <- liftThrows $ eqv [selector,choice]
  case comparison of
    Bool True -> eval env conseq
    Bool False -> myCaseFun env rest selector
  where
    caseHasAtom (Atom _) = True
    caseHasAtom _        = False

myIfFun :: Env -> LispVal -> LispVal -> LispVal -> IOThrowsError LispVal
myIfFun env pred conseq alt = do
  result <- eval env pred
  case result of
    Bool False -> eval env alt
    Bool True  -> eval env conseq
    otherwise  -> liftThrows . throwError $ TypeMismatch "boolean" pred

myCondFun :: Env -> [LispVal] -> IOThrowsError LispVal
myCondFun _ [] = liftThrows . throwError $ BadSpecialForm "Non-exhaustive patterns in" $ String "cond"
myCondFun env [Atom "otherwise", conseq] = eval env conseq
myCondFun env (pred:conseq:rest) = do
  result <- eval env pred
  case result of
    Bool True  -> eval env conseq
    Bool False -> myCondFun env rest
    otherwise  -> liftThrows . throwError $ TypeMismatch "boolean" pred

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
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

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("apply", applyProc),
                ("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-port", closePort),
                ("close-output-port", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("read-all", readAll)]

subStringFn :: [LispVal] -> ThrowsError LispVal
subStringFn [String s, Number start, Number end]
  | indicesValid = (return . String) $ genericSubList s
  | otherwise = throwError $ Default $ "substring: indices out of range for input: " ++ s
  where
    indicesValid = 0 <= start && start <= end && end <= L.genericLength s
    genericSubList = L.genericTake (end - start) . L.genericDrop start
-- Provide some at least slightly useful errors.
subStringFn [s, n, k] = throwError $ TypeMismatches "target (string) start (number) end (number)" [s, n, k]
subStringFn badArgList = throwError $ NumArgs 3 badArgList

integerCount :: [a] -> Int
integerCount = foldl (\n x -> n + 1) 0

stringinsertFn :: [LispVal] -> ThrowsError LispVal
stringinsertFn [String s, Number k, Character c] =
  if integerCount s >= idx
    then (return . String) $ x ++ [c] ++ xs
    else throwError $ Default "string-insert!: index out of range!"
  where
    idx = fromInteger k
    (x,xs) = splitAt idx s
stringSetFn badArgList = throwError $ NumArgs 3 badArgList

stringRefFn :: [LispVal] -> ThrowsError LispVal
stringRefFn [String s, Number n] = (return . Character) $ L.genericIndex s n
stringRefFn [String _, badArg]   = throwError $ TypeMismatch "number" badArg
stringRefFn [badArg, Number _]   = throwError $ TypeMismatch "string" badArg
stringRefFn badArgList           = throwError $ NumArgs 2 badArgList

buildString :: String -> [LispVal] -> String
buildString acc [] = acc
buildString acc (Character c : rest) = buildString (acc ++ [c]) rest

makeStringFromArgs :: [LispVal] -> ThrowsError LispVal
makeStringFromArgs val@(Character _ :_) = (return . String) $ buildString [] val
makeStringFromArgs badArg               = throwError $ NumArgs 2 badArg

stringLength :: [LispVal] -> ThrowsError LispVal
stringLength [String val] = (return . Number) $ L.genericLength val
stringLength [badArg]     = throwError $ TypeMismatch "string" badArg
stringLength badArgList   = throwError $ NumArgs 1 badArgList

makeStringN :: [LispVal] -> ThrowsError LispVal
makeStringN [Number n, Character c] = (return . String . L.genericTake n) $ repeat c
makeStringN [Number n, _]           = (return . String . L.genericTake n) ['a'..]
makeStringN [bad, _]                = throwError $ TypeMismatch "number [char]" bad
makeStringN badArgList              = throwError $ NumArgs 1 badArgList

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op parms = liftM (Number . foldl1 op) (mapM unpackNum parms)

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ head args
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

leespHead :: [LispVal] -> ThrowsError LispVal
leespHead [List (x:xs)]         = return x
leespHead [DottedList (x:xs) _] = return x
leespHead [badArg]              = throwError $ TypeMismatch "pair" badArg
leespHead badArgList            = throwError $ NumArgs 1 badArgList

leespRest :: [LispVal] -> ThrowsError LispVal
leespRest [DottedList (_ : xs) x] = return $ DottedList xs x
leespRest [DottedList [xs] x]     = return x
leespRest [List (x:xs)]           = return $ List xs
leespRest [badArg]                = throwError $ TypeMismatch "pair" badArg
leespRest badArgList              = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []]            = return $ List [x1]
cons [x, List xs]             = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2]                 = return $ DottedList [x1] x2
cons badArgList               = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [Bool arg1, Bool arg2]             = return $ Bool $ arg1 == arg2
eqv [Number arg1, Number arg2]         = return $ Bool $ arg1 == arg2
eqv [String arg1, String arg2]         = return $ Bool $ arg1 == arg2
eqv [Atom arg1, Atom arg2]             = return $ Bool $ arg1 == arg2
eqv [DottedList xs x, DottedList ys y] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [List arg1, List arg2]             = return
  $ Bool
  $ (length arg1 == length arg2) && and (zipWith (curry eqvPair) arg1 arg2)
  where
    eqvPair (x1, x2) = case eqv [x1, x2] of
                        Left err -> False
                        Right (Bool val) -> val
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
  do unpacked1 <- unpacker arg1
     unpacked2 <- unpacker arg2
     return $ unpacked1 == unpacked2
 `catchError` const (return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
  primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
                     [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
  eqvEquals <- eqv [arg1, arg2]
  return $ Bool (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

-- START STATE MGMT
nullEnv :: IO Env
nullEnv = newIORef []

makeFunc :: Monad m => Maybe String -> Env -> [LispVal] -> [LispVal] -> m LispVal
makeFunc varargs env parms body = return $ Func (map showVal parms) varargs body env

makeNormalFunc :: Env -> [LispVal] -> [LispVal] -> ErrorT LispError IO LispVal
makeNormalFunc = makeFunc Nothing

makeVarargs :: LispVal -> Env -> [LispVal] -> [LispVal] -> ErrorT LispError IO LispVal
makeVarargs = makeFunc . Just . showVal

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = liftM extractValue (runErrorT (trapError action))

isBound :: Env -> String -> IO Bool
-- isBound envRef var = readIORef envRef >>= return . isJust . lookup var
isBound envRef var = liftM (isJust . lookup var) (readIORef envRef)

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Getting an unbound variable" var)
        (liftIO . readIORef)
        (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Setting an unbound variable" var)
        (liftIO . (`writeIORef` value))
        (lookup var env)
  return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
    then setVar envRef var value >> return value
    else liftIO $ do
      valueRef <- newIORef value
      env <- readIORef envRef
      writeIORef envRef ((var, valueRef) : env)
      return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where
    extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
    addBinding (var, value) = do
      ref <- newIORef value
      return (var, ref)

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= flip bindVars (map (makeFunc IOFunc) ioPrimitives
  ++ map (makeFunc PrimitiveFunc) primitives)
  where
    makeFunc constructor (var, func) = (var, constructor func)

-- START IO PRIMITIVES
applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO (hClose port) >> return (Bool True)

readProc :: [LispVal] -> IOThrowsError LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = liftIO (hGetLine port) >>= liftThrows . readExpr

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> return (Bool True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename

load :: String -> IOThrowsError [LispVal]
load filename = liftIO (readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename
-- END IO PRIMITIVES
