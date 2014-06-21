{-# LANGUAGE UnicodeSyntax #-}

module Main where

-- For genericN functions
import qualified Data.List as L

-- For the REPL
import System.IO hiding (try)

-- For parsing/eval
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Error

-- Required for general operations.
import Data.Functor ((<$>))
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

-- Required for managing state
import Data.IORef
import qualified Data.Map as Map
import Data.Maybe (isJust, isNothing, fromJust)

-- Symbols
import Data.Eq.Unicode
import Data.Bool.Unicode
import Data.Function.Unicode 

import LeespTypes
import LeespParser

main ∷ IO ()
main = do
  args ← getArgs
  if null args
    then runRepl
    else runOne args

-- REPL FUNS
flushStr ∷ String → IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt ∷ String → IO String
readPrompt prompt = flushStr prompt >> getLine

-- evalString ∷ Env → String → IO String
-- evalString env expr = runIOThrows $ liftM show $ liftThrows (readExpr expr) >>= eval env

evalString ∷ Env → String → String
evalString env str = case readExpr str of
  Left err → show err
  Right exp → case runEval env env (eval exp) of
    (Left err, s) → show err
    (Right v, s) → show v

evalAndPrint ∷ Env → String → IO ()
evalAndPrint env expr = putStrLn $ evalString env expr

until_ :: Monad m ⇒ (a → Bool) → m a → (a → m ()) → m ()
until_ pred prompt action = do result ← prompt
                               unless (pred result) $ action result >> until_ pred prompt action

-- This version of runOne expects a file input to read from.
runOne ∷ [String] → IO ()
runOne args = evalAndPrint newEnv $ "(import " ++ head args ++ ")"
  where
    newEnv = flip bindVars [("args", List $ map String $ drop 1 args)] $ primitiveBindings

runRepl ∷ IO ()
runRepl = do
  displayReplBanner
  until_ (≡ "quit") (readPrompt "Leesp >> ") $ evalAndPrint primitiveBindings

displayReplBanner ∷ IO ()
displayReplBanner =
  let bannerLines = ["Welcome to the Leesp REPL!",
                     "You can import leebs by using the (import \"path/to/your/leeb\")",
                     "To quit just type in 'quit'.",
                     "Multiline expressions are not yet supported, sorry. :("]
  in putStrLn `mapM_` bannerLines

-- END REPL FUNS

readOrThrow ∷ Parser a → String → ThrowsError a
readOrThrow parser input = case parse parser "leesp" input of
  Left err → throwError $ Parser err
  Right val → return val

readExpr ∷ String → ThrowsError LispVal
readExpr = readOrThrow parseExpr

readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow (endBy parseExpr (comments <|> spaces))

apply ∷ LispVal → [LispVal] → Eval LispVal
apply (PrimitiveFunc fun) args = case fun args of
  Left err → throwError err
  Right val → return val
apply (Func parms vargs body closure) args =
  if intLen parms ≢ intLen args ∧ isNothing vargs
  then throwError $ NumArgs (intLen parms) args
  else do ev' ← bindVarArgs vargs $ bindVars closure zippedArgs
          case evalBody ev' body of
            (Left err, badState) → throwError err
            (Right val, goodState) → return val
  where
    intLen = toInteger ∘ length    
    remaining = drop (length parms) args
    zippedArgs = zip parms args
    bindVarArgs arg env = case arg of
      Just argName -> return $ bindVars env [(argName, List remaining)]
      Nothing -> return env
    evalBody env = last ∘ map (\expr → runEval env env (eval expr))

eval ∷ LispVal → Eval LispVal
eval val@(String _)               = return val
eval val@(Number _)               = return val
eval val@(Bool _)                 = return val
eval val@(Character _)            = return val
eval val@(Keyword _)              = return val
eval (Atom id) = do env ← ask
                    case Map.lookup id env of
                      Nothing → throwError $ UnboundVar "unbound var" id
                      Just val → return val
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
eval (List (Atom "lambda" : DottedList parms varargs : body)) = makeVarArgs varargs parms body
eval (List (Atom "lambda" : varargs@(Atom _) : body)) = makeVarArgs varargs [] body
eval (List (Atom "lambda" : List parms : body)) = makeNormalFunc parms body
eval (List (function : args)) = do
  func ← eval function
  argVals ← mapM (eval) args
  apply func argVals
-- Yer done gone f**ked up.
eval badForm = throwError $ BadSpecialForm "Unrecognised special form" badForm

makeFunc ∷ Maybe String → [LispVal] → [LispVal] → Eval LispVal
makeFunc vargs parms body = do env ← ask
                               return (Func (map showVal parms) vargs body env)

makeNormalFunc ∷ [LispVal] → [LispVal] → Eval LispVal
makeNormalFunc = makeFunc Nothing

makeVarArgs ∷ LispVal → [LispVal] → [LispVal] → Eval LispVal
makeVarArgs = makeFunc ∘ Just ∘ showVal

testingEval ∷ (Either LispError LispVal, Env)
testingEval = runEval env env (eval exp)
  where
    env = emptyEnv
    exp = (List [(Atom "define"), (Atom "foo"),
            (List [(Atom "lambda"),
                   (List [(String "a"),(String "b")]),
                   (List [(Atom "+"),(Atom "a"),(Atom "b")])])])

evIfFun ∷ LispVal → LispVal → LispVal → Eval LispVal
evIfFun pred conseq alt =
  do p ← eval pred
     case p of
       Bool True → eval conseq
       Bool False → eval alt
       _ → throwError $ TypeMismatch "Non boolean result in 'if'" p

evCondFun ∷ [LispVal] → Eval LispVal
evCondFun [] = throwError $ BadSpecialForm "Non-exhaustive patterns in" $ String "cond"
evCondFun [Atom "otherwise", conseq] = eval conseq
evCondFun (pred:conseq:rest) = do
  result ← eval pred
  case result of
    Bool True  → eval conseq
    Bool False → evCondFun rest
    otherwise  → throwError $ TypeMismatch "boolean" pred
    
evCaseFun ∷ [LispVal] → LispVal → Eval LispVal
evCaseFun [] selector = throwError $ BadSpecialForm "Non-exhaustive patterns in " selector
evCaseFun (comparator:conseq:rest) selector = do
  choice ← if caseHasAtom comparator then return comparator else eval comparator
  equiv ← eqv [selector,choice]
  case equiv of
    Bool True → eval conseq
    Bool False → evCaseFun rest selector
  where
    caseHasAtom (Atom _) = True
    caseHasAtom _        = False
        
  --   Left _ → throwError $ BadSpecialForm "Unknown Pattern in Case form " selector
  --   Right v → case v of 
  --     Bool True → eval conseq
  --     Bool False → evCaseFun rest selector
  -- where

primitives ∷ [(String, [LispVal] → Eval LispVal)]
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

ioPrimitives ∷ [(String, [LispVal] → Eval LispVal)]
ioPrimitives = [("apply", applyProc),
                ("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-port", closePort),
                ("close-output-port", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("read-all", readAll)]

subStringFn ∷ [LispVal] → Eval LispVal
subStringFn [String s, Number start, Number end]
  | indicesValid = (return ∘ String) $ genericSubList s
  | otherwise = throwError $ Default $ "substring: indices out of range for input: " ++ s
  where
    indicesValid = 0 <= start && start <= end && end <= L.genericLength s
    genericSubList = L.genericTake (end - start) . L.genericDrop start
-- Provide some at least slightly useful errors.
subStringFn [s, n, k] = throwError $ TypeMismatches "target (string) start (number) end (number)" [s, n, k]
subStringFn badArgList = throwError $ NumArgs 3 badArgList

integerCount ∷ [a] → Int
integerCount = foldl (\n x → n + 1) 0

stringinsertFn [String s, Number k, Character c] =
  if integerCount s >= idx
    then (return . String) $ x ++ [c] ++ xs
    else throwError $ Default "string-insert!: index out of range!"
  where
    idx = fromInteger k
    (x,xs) = splitAt idx s
stringSetFn badArgList = throwError $ NumArgs 3 badArgList

stringRefFn ∷ [LispVal] → Eval LispVal
stringRefFn [String s, Number n] = (return . Character) $ L.genericIndex s n
stringRefFn [String _, badArg]   = throwError $ TypeMismatch "number" badArg
stringRefFn [badArg, Number _]   = throwError $ TypeMismatch "string" badArg
stringRefFn badArgList           = throwError $ NumArgs 2 badArgList

buildString ∷ String → [LispVal] -> String
buildString acc [] = acc
buildString acc (Character c : rest) = buildString (acc ++ [c]) rest

makeStringFromArgs ∷ [LispVal] → Eval LispVal
makeStringFromArgs val@(Character _ :_) = (return . String) $ buildString [] val
makeStringFromArgs badArg               = throwError $ NumArgs 2 badArg

stringLength ∷ [LispVal] → Eval  LispVal
stringLength [String val] = (return . Number) $ L.genericLength val
stringLength [badArg]     = throwError $ TypeMismatch "string" badArg
stringLength badArgList   = throwError $ NumArgs 1 badArgList

makeStringN ∷ [LispVal] → Eval LispVal
makeStringN [Number n, Character c] = (return ∘ String ∘ L.genericTake n) $ repeat c
makeStringN [Number n, _]           = (return ∘ String ∘ L.genericTake n) ['a'..]
makeStringN [bad, _]                = throwError $ TypeMismatch "number [char]" bad
makeStringN badArgList              = throwError $ NumArgs 1 badArgList

numericBinop ∷ (Integer → Integer → Integer) → [LispVal] → Eval LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op parms = (Number ∘ foldl1 op) <$> map (unpackNum) parms

unpackNum ∷ LispVal → Integer
unpackNum (Number n) = n
--unpackNum notNum = throwError $ TypeMismatch "number" notNum

boolBinop ∷ (LispVal → ThrowsError a) → (a → a → Bool) → [LispVal] → Eval LispVal
boolBinop unpacker op args = if length args ≢ 2
                             then throwError $ NumArgs 2 args
                             else do left ← unpacker $ head args
                                     right ← unpacker $ args !! 1
                                     return $ Bool $ left `op` right

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

unpackStr ∷ LispVal → ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
-- unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool ∷ LispVal → ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

leespHead ∷ [LispVal] → Eval LispVal
leespHead [List (x:xs)]         = return x
leespHead [DottedList (x:xs) _] = return x
leespHead [badArg]              = throwError $ TypeMismatch "pair" badArg
leespHead badArgList            = throwError $ NumArgs 1 badArgList

leespRest ∷ [LispVal] → Eval LispVal
leespRest [DottedList (_ : xs) x] = return $ DottedList xs x
leespRest [DottedList [xs] x]     = return x
leespRest [List (x:xs)]           = return $ List xs
leespRest [badArg]                = throwError $ TypeMismatch "pair" badArg
leespRest badArgList              = throwError $ NumArgs 1 badArgList

cons ∷ [LispVal] → Eval LispVal
cons [x1, List []]            = return $ List [x1]
cons [x, List xs]             = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2]                 = return $ DottedList [x1] x2
cons badArgList               = throwError $ NumArgs 2 badArgList

eqv ∷ [LispVal] → Eval LispVal
eqv [Bool arg1, Bool arg2]             = return $ Bool $ arg1 == arg2
eqv [Number arg1, Number arg2]         = return $ Bool $ arg1 == arg2
eqv [String arg1, String arg2]         = return $ Bool $ arg1 == arg2
eqv [Atom arg1, Atom arg2]             = return $ Bool $ arg1 == arg2
eqv [DottedList xs x, DottedList ys y] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [List arg1, List arg2]             = return
  $ Bool
  $ (length arg1 ≡ length arg2) ∧ and (zipWith (curry eqvPair) arg1 arg2)
  where
    eqvPair (x1, x2) = case eqv [x1, x2] of
                        Left err → False
                        Right (Bool val) → val
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

data Unpacker = forall a. Eq a ⇒ AnyUnpacker (LispVal → Eval a)

unpackEquals ∷ LispVal → LispVal → Unpacker → ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
  do unpacked1 ← unpacker arg1
     unpacked2 ← unpacker arg2
     return $ unpacked1 ≡ unpacked2
 `catchError` const (return False)

equal ∷ [LispVal] → Eval LispVal
equal [arg1, arg2] = do
  primitiveEquals ← or <$> mapM (unpackEquals arg1 arg2)
                     [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
  eqvEquals ← eqv [arg1, arg2]
  return $ Bool (primitiveEquals ∨ let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

-- START STATE MGMT
emptyEnv ∷ Env
emptyEnv = Map.empty

liftThrows ∷ ThrowsError a → IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows ∷ IOThrowsError String → IO String
runIOThrows action = liftM extractValue (runErrorT (trapError action))

setVar ∷ String → LispVal → Eval LispVal
setVar var value = do env ← ask
                      case Map.lookup var env of
                        Nothing → defineVar var value
                        Just _ → throwError $ BoundVar "Variable already bound" value

defineVar ∷ String → LispVal → Eval LispVal
defineVar var value = modify (Map.insert var value) >> return value

bindVars ∷ Env → [(String,LispVal)] → Env
bindVars envM bindings = Map.union envM $ Map.fromList bindings

primitiveBindings ∷ Env
primitiveBindings = emptyEnv >>= flip bindVars prims
  where
    mkFunc constr (var,fn) = (var, constr fn)
    mapMkFun t = map (mkFunc t)
    prims = (mapMkFun (IOFunc) ioPrimitives) ++ (mapMkFun (PrimitiveFunc) primitives)

-- START IO PRIMITIVES
applyProc ∷ [LispVal] → Eval LispVal
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args

makePort ∷ IOMode → [LispVal] → Eval LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

closePort ∷ [LispVal] → Eval LispVal
closePort [Port port] = liftIO (hClose port) >> return (Bool True)

readProc ∷ [LispVal] → Eval LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = liftIO (hGetLine port) >>= liftThrows ∘ readExpr

writeProc ∷ [LispVal] → Eval LispVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> return (Bool True)

readContents ∷ [LispVal] → Eval LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename

load ∷ String → IO [LispVal]
load filename = liftIO (readFile filename) >>= liftThrows ∘ readExprList

readAll ∷ [LispVal] → Eval LispVal
readAll [String filename] = liftM List $ load filename
-- END IO PRIMITIVES
