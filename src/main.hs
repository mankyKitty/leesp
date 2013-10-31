module Main where

-- For genericN functions
import qualified Data.List as L
-- For the REPL
import System.IO hiding (try)
-- For parsing/eval
import Control.Monad (liftM)
import Control.Monad.Error
import Numeric (readOct, readHex)
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

main :: IO ()
main = do args <- getArgs
          case length args of
            0 -> runRepl
            1 -> evalAndPrint $ args !! 0
            _ -> putStrLn "Program takes only 0 or 1 argument"
	--evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
	--putStrLn $ extractValue $ trapError evaled

-- REPL FUNS
flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: String -> IO String
evalString expr = return $ extractValue $ trapError (liftM show $ readExpr expr >>= eval)

evalAndPrint :: String -> IO ()
evalAndPrint expr = evalString expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
    then return ()
    else action result >> until_ pred prompt action

runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "Leesp>>> ") evalAndPrint
-- END REPL FUNS

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~"

spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err  -> throwError $ Parser err
  Right val -> return val

eval :: LispVal -> ThrowsError LispVal
-- Display Evaluated Values.
eval val@(String _)                        = return val
eval val@(Number _)                        = return val
eval val@(Bool _)                          = return val
eval val@(Character _)                     = return val
-- Handled Quoting.
eval (List [Atom "quote", val])            = return val
-- Flow Control Functions.
eval (List [Atom "if", pred, conseq, alt]) = myIfFun pred conseq alt
eval (List (Atom "cond" : items))          = myCondFun items
eval (List (Atom "case" : sel : choices))  = eval sel >>= myCaseFun choices
-- Evaluate Function Section.
eval (List (Atom func : args))             = mapM eval args >>= apply func
-- Yer done gone f**ked up.
eval badForm                               = throwError $ BadSpecialForm "Unrecognised special form" badForm

myCaseFun :: [LispVal] -> LispVal -> ThrowsError LispVal
myCaseFun [] selector = throwError $ BadSpecialForm "Non-exhaustive patterns in " selector
myCaseFun (comparator:conseq:rest) selector = do
  choice <- if caseHasAtom comparator
            then return comparator
            else eval comparator
  comparison <- eqv [selector,choice]
  case comparison of
    Bool True -> eval conseq
    Bool False -> myCaseFun rest selector
  where
    caseHasAtom (Atom _) = True
    caseHasAtom _        = False

myIfFun :: LispVal -> LispVal -> LispVal -> ThrowsError LispVal
myIfFun pred conseq alt = do
  result <- eval pred
  case result of
    Bool False -> eval alt
    Bool True  -> eval conseq
    otherwise  -> throwError $ TypeMismatch "boolean" pred

myCondFun :: [LispVal] -> ThrowsError LispVal
myCondFun [] = throwError $ BadSpecialForm "Non-exhaustive patterns in" $ String "cond"
myCondFun [Atom "otherwise", conseq] = eval conseq
myCondFun (pred:conseq:rest) = do
  result <- eval pred
  case result of
    Bool True  -> eval conseq
    Bool False -> myCondFun rest
    otherwise  -> throwError $ TypeMismatch "boolean" pred

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognised primitive function args" func)
					    ($ args)
					    (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
        -- Basic Maths Functions
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
        ("car", car),
        ("cdr", cdr),
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

subStringFn :: [LispVal] -> ThrowsError LispVal
subStringFn [String s, Number start, Number end]
  | indicesValid = (return . String) $ genericSubList s
  | otherwise = throwError $ Default $ "substring: indices out of range for input: " ++ s
  where
    indicesValid = and [0 <= start, start <= end, end <= L.genericLength s]
    genericSubList = (L.genericTake (end - start) . L.genericDrop start)
-- Provide some at least slightly useful errors.
subStringFn [s, n, k] = throwError $ TypeMismatches "target (string) start (number) end (number)" [s, n, k]
subStringFn badArgList = throwError $ NumArgs 3 badArgList

integerCount :: [a] -> Int -> Int
integerCount [] n = n
integerCount (x:xs) n = integerCount xs (n + 1)

stringinsertFn :: [LispVal] -> ThrowsError LispVal
stringinsertFn [String s, Number k, Character c] =
  if integerCount s 0 >= idx
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

buildString :: [Char] -> [LispVal] -> [Char]
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
makeStringN [Number n, _] = (return . String . L.genericTake n) ['a'..]
makeStringN [bad, _] = throwError $ TypeMismatch "number [char]" bad
makeStringN badArgList = throwError $ NumArgs 1 badArgList

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ args !! 0
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

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Character Char

data LispError = NumArgs Integer [LispVal]
			   | TypeMismatch String LispVal
         | TypeMismatches String [LispVal]
			   | Parser ParseError
			   | BadSpecialForm String LispVal
			   | NotFunction String String
			   | UnboundVar String String
			   | Default String

instance Show LispError where show = showError

instance Error LispError where
	noMsg = Default "An error has occurred"
	strMsg = Default

type ThrowsError = Either LispError

instance Show LispVal where show = showVal

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

parseCharacter :: Parser LispVal
parseCharacter = do
  try $ string "#\\"
  value <- try (string "newline" <|> string "space")
           <|> do { x <- anyChar; notFollowedBy alphaNum ; return [x] }
  return $ Character $ case value of
    "space" -> ' '
    "newline" -> '\n'
    otherwise -> (value !! 0)

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
parseBool = do
  char '#'
  (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))


parseString :: Parser LispVal
parseString = do char '"'
                 x <- many (escapeChars <|> noneOf "\"\\")
                 char '"'
                 return $ String x

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = [first] ++ rest
               return $ case atom of
                          "#t" -> Bool True
                          "#f" -> Bool False
                          otherwise -> Atom atom

parseNumber :: Parser LispVal
parseNumber = parseDigital1 <|> parseDigital2 <|> parseHex <|> parseOct <|> parseBin

parseDigital1 :: Parser LispVal
parseDigital1 = many1 digit >>= (return . Number . read)

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
        <|> try parseNumber
        <|> try parseBool
        <|> try parseCharacter
        <|> parseQuoted
        <|> do char '('
               x <- (try parseList) <|> parseDottedList
               char ')'
               return x

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

car :: [LispVal] -> ThrowsError LispVal
car [List (x:xs)]         = return x
car [DottedList (x:xs) _] = return x
car [badArg]              = throwError $ TypeMismatch "pair" badArg
car badArgList            = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [DottedList [xs] x]     = return x
cdr [List (x:xs)]           = return $ List xs
cdr [badArg]                = throwError $ TypeMismatch "pair" badArg
cdr badArgList              = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []]            = return $ List [x1]
cons [x, List xs]             = return $ List $ [x] ++ xs
cons [x, DottedList xs xlast] = return $ DottedList ([x] ++ xs) xlast
cons [x1, x2]                 = return $ DottedList [x1] x2
cons badArgList               = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)]             = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)]         = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)]         = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)]             = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)]             = return $ Bool $ (length arg1 == length arg2) && (and $ map eqvPair $ zip arg1 arg2)
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
 `catchError` (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
  primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2) [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
  eqvEquals <- eqv [arg1, arg2]
  return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList
