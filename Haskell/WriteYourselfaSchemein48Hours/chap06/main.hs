{-# LANGUAGE ExistentialQuantification #-}
module Main where
import System.IO
import System.Environment
import Text.ParserCombinators.Parsec
import Control.Monad
import Control.Monad.Error
import Data.Char
import Numeric
import Data.Bool
import Data.Ratio
import Data.Complex
import Data.Array
import Prelude

{-- BEGIN REPL --}

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: String -> IO String
evalString expr = return $ extractValue $ trapError (liftM show $ readExpr expr >>= eval)

evalAndPrint :: String -> IO ()
evalAndPrint expr = evalString expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred  prompt action = do
  result <- prompt
  if pred result
    then return ()
    else action result >> until_ pred prompt action

runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt ">>> ") evalAndPrint

{-- END REPL --}


data LispVal = Atom String
                | List [LispVal]
                | DottedList [LispVal] LispVal
                | Vector (Array Int LispVal)
                | String String
                | Number Integer
                | Float Double
                | Complex (Complex Double)
                | Ratio Rational
                | Bool Bool
                | Character Char
                deriving (Eq)

instance Show LispVal where show = showVal



data LispError = NumArgs Integer [LispVal]
                | TypeMismatch String LispVal
                | Parser ParseError
                | BadSpecialForm String LispVal
                | NotFunction String String
                | UnboundVar String String
                | Default String

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

instance Show LispError where show = showError

instance Error LispError where
    noMsg = Default "An error has occured"
    strMsg = Default

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a-> a
extractValue (Right val) = val


main :: IO ()
main = do
           args <- getArgs
           case length args of
             0 -> runRepl
             1 -> evalAndPrint $ args !! 0
             otherwise -> putStrLn "Program takes only 0 or 1 argument"
readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val -- "Found " ++ show val





{-- BEGIN PARSER --}

symbol :: Parser Char
symbol= oneOf "!$%&|*+-/:<=>?@^_~"

spaces1 :: Parser ()
spaces1 = skipMany1 space

parseExpr :: Parser LispVal
parseExpr = parseAtom
           <|> parseString
           <|> try parseCharacter
           <|> parseVector
           <|> try parseComplex
           <|> try parseFloat
           <|> try parseRatio
           <|> try parseNumber
           <|> parseBool
           <|> parseQuoted
           <|> parseQuasiQuited
           <|> parseUnQuote
           <|> parseList
{--
 -
 - PARSING STRING
 -
--}

escapedChars :: Parser Char
escapedChars = do
                x <- char '\\' >> oneOf "\\\"nrt"
                return $ case x of
                    'n' -> '\n'
                    'r' -> '\r'
                    't' -> '\t'
                    _   -> x

parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many $ escapedChars <|> noneOf "\"\\"
                char '"'
                return $ String x
{--
 -
 - PARSING ATOM
 -
--}

parseAtom :: Parser LispVal
parseAtom = do
              first <- letter <|> symbol
              rest <- many (letter <|> digit <|> symbol)
              let atom = first:rest
              return $ case atom of
                        "#t" -> Bool True
                        "#f" -> Bool False
                        _    -> Atom atom

{--
 -
 - PARSING NUMBER
 -
--}

-- parseNumber :: Parser LispVal
-- parseNumber = liftM (Number . read) $ many1 digit

-- parseNumber :: Parser LispVal
-- parseNumber = do
--                x <- many1 digit
--                (return . Number . read ) x


-- parseNumber :: Parser LispVal
-- parseNumber =  many1 digit >>= return . Number . read

parseDigital1 :: Parser LispVal
parseDigital1 = do
                    x <- many1 digit
                    (return . Number . read) x

parseDigital2 :: Parser LispVal
parseDigital2 = do
                    try $ string "#d"
                    x <- many1 digit
                    (return . Number . read) x

parseHex :: Parser LispVal
parseHex = do
                try $ string "#x"
                x <- many1 hexDigit
                return $ Number (hex2dig x)

parseOct :: Parser LispVal
parseOct = do
                try $ string "#o"
                x <- many1 octDigit
                return $ Number (oct2dig x)

parseBin :: Parser LispVal
parseBin = do
                try $ string "#b"
                x <- many1 (oneOf "10")
                return $ Number (bin2dig x)

hex2dig x = fst $ readHex x !! 0
oct2dig x = fst $ readOct x !! 0
bin2dig = bin2dig' 0
bin2dig' digint "" = digint
bin2dig' digint (x:xs) = let old = 2 * digint + (if x == '0' then 0 else 1) in bin2dig' old xs

parseNumber :: Parser LispVal
parseNumber = do
               num <- parseDigital1 <|> parseDigital2 <|> parseHex <|> parseOct <|> parseBin
               return num

parseFloat :: Parser LispVal
parseFloat = do x <- many1 digit
                char '.'
                y <- many1 digit
                return $ Float (read (x++"."++y))

parseRatio :: Parser LispVal
parseRatio = do x <- many1 digit
                char '/'
                y <- many1 digit
                return $ Ratio ((read x) % (read y))

toDouble :: LispVal -> Double
toDouble(Float f) = realToFrac f
toDouble(Number n) = fromIntegral n

parseComplex :: Parser LispVal
parseComplex = do x <- (try parseFloat <|> parseDigital1)
                  char '+'
                  y <- (try parseFloat <|> parseDigital1)
                  char 'i'
                  return $ Complex(toDouble x :+ toDouble y)

{--
 -
 - PARSING BOOLEAN
 -
--}

parseBool :: Parser LispVal
parseBool = do
            string "#"
            x <- oneOf "tf"
            return $ case x of
                        't' -> Bool True
                        'f' -> Bool False

{--
 -
 - PARSING CHARACTER
 -
--}

parseCharacter :: Parser LispVal
parseCharacter = do
                try $ string "#\\"
                value <- try (string "newline" <|> string "space")
                         <|> do {x <- anyChar; notFollowedBy alphaNum ; return [x]}
                return $ Character $ case value of
                    "space" -> ' '
                    "newline" -> '\n'
                    otherwise -> (value !! 0)

{--
 -
 - PARSING LIST
 -
--}
parseList :: Parser LispVal
parseList = do char '(' >> spaces
               head <- parseExpr `sepEndBy` spaces1
               do char '.' >> spaces1
                  tail <- parseExpr
                  spaces >> char ')'
                  return $ DottedList head tail
                <|> (spaces >> char ')' >> (return $ List head))

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

parseQuasiQuited :: Parser LispVal
parseQuasiQuited = do
                        char '`'
                        x <- parseExpr
                        return $ List [Atom "quasiquoted", x]

parseUnQuote :: Parser LispVal
parseUnQuote = do
                char ','
                x <- parseExpr
                return $ List [Atom "unquote", x]

{--
 -
 - PARSING VECTOR
 -
--}

parseVector :: Parser LispVal
parseVector = do string "#("
                 elems <- sepBy parseExpr spaces1
                 char ')'
                 return $ Vector (listArray (0, length elems-1) elems)
{-- END PARSER --}

-- ============================================================================================

{-- BEGIN EVALUATOR --}
showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Float contents) = show contents
showVal (Complex contents) = show contents
showVal (Ratio contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

eval:: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Float _) = return val
eval val@(Complex _) = return val
eval val@(Ratio _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
{-- eval (List [Atom "if", pred, conseq, alt]) = do result <- eval pred
                                                case result of
                                                  Bool False -> eval alt
                                                  otherwise -> eval conseq
--}
eval (List [Atom "if", pred, conseq, alt]) = do result <- eval pred
                                                case result of
                                                    Bool True -> eval conseq
                                                    Bool False -> eval alt
                                                    _ -> throwError $ TypeMismatch "bool" pred


eval (List ((Atom "cond"):cs)) = do
  b <- (liftM (take 1 . dropWhile f) $ mapM condClause cs) >>= cdr
  car [b] >>= eval
    where condClause (List [p, b]) = do q <- eval p
                                        case q of
                                          Bool _ -> return $ List [q, b]
                                          _      -> throwError $ TypeMismatch "bool" q

          condClause v              = throwError $ TypeMismatch "(pred body)" v
          f                         = \(List [p,b]) -> case p of
                                                        (Bool False) -> True
                                                        _            -> False


eval form@(List (Atom "case" : key : clauses)) =
  if null clauses
  then throwError $ BadSpecialForm "no true clause in case expression: " form
  else case head clauses of
    List (Atom "else" : exprs) -> mapM eval exprs >>= return . last
    List ((List datums) : exprs) -> do
      result <- eval key
      equality <- mapM (\x -> eqv [result, x]) datums
      if Bool True `elem` equality
        then mapM eval exprs >>= return . last
        else eval $ List (Atom "case" : key : tail clauses)
    _                 -> throwError $ BadSpecialForm "ill-formed case expression: " form


eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm


apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitives function args" func)
                        ($ args)
                        (lookup func primitives)



primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("symbol?", unaryOp symbolp),
              ("string?", unaryOp stringp),
              ("number?", unaryOp numberp),
              ("bool?", unaryOp boolp),
              ("list?", unaryOp listp),
              ("symbol->string", unaryOp symbol2string),
              ("string->symbol", unaryOp string2symbol),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal)]

--
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op



unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
                            if null parsed
                                then throwError $ TypeMismatch "number" $ String n
                                else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

--
unaryOp :: (LispVal -> LispVal) -> [LispVal] ->ThrowsError LispVal
unaryOp f [v] = return $ f v

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
unpackStr (Bool s) = return $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool


symbolp, numberp, stringp, boolp, listp :: LispVal -> LispVal
symbolp (Atom _) =  Bool True
symbolp _ = Bool False
numberp (Number _) = Bool True
numberp _ = Bool False
stringp (String _) = Bool True
stringp _ = Bool False
boolp (Bool _) = Bool True
boolp _ = Bool False
listp (List _) = Bool True
listp (DottedList _ _) = Bool True
listp _ = Bool False

symbol2string, string2symbol :: LispVal -> LispVal
symbol2string (Atom s) =  String s
symbol2string _ = String ""
string2symbol (String s) = Atom s
string2symbol _ = Atom ""



{-- END EVALUATOR --}


{-- PRIMITIVES --}
car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)] = return x
car [DottedList (x : xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)] = return $ List xs
cdr [DottedList [xs] x] = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)] = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)] = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)] = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)] = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)] = return $ Bool $ (length arg1 == length arg2) && (all eqvPair $ zip arg1 arg2)
                                where eqvPair (x1, x2) = case eqv [x1,x2] of
                                                          Left err -> False
                                                          Right (Bool val) -> val
eqv [l1@(List arg1), l2@(List arg2)] = eqvList eqv [l1, l2]
eqv [_, _] = return  $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)
unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
      do unpacked1 <- unpacker arg1
         unpacked2 <- unpacker arg2
         return $ unpacked1 == unpacked2
      `catchError` (const $ return False)
{--
equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
  primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
                     [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
  eqvEquals <- eqv [arg1, arg2]
  return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList
--}

eqvList :: ([LispVal] -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
eqvList eqvFunc [(List arg1), (List arg2)] = return $ Bool $ (length arg1 == length arg2) &&
                                                    (all eqvPair $ zip arg1 arg2)
        where eqvPair (x1, x2) = case eqvFunc [x1, x2] of
                                    Left err -> False
                                    Right (Bool val) -> val


equal :: [LispVal] -> ThrowsError LispVal
equal [l1@(List arg1), l2@(List arg2)] = eqvList equal[l1, l2]
equal [(DottedList xs x), (DottedList ys y)] = equal [List $ xs ++ [x], List $ ys ++ [y]]
equal [arg1, arg2] = do
  primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
                     [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
  eqvEquals <- eqv [arg1, arg2]
  return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList
