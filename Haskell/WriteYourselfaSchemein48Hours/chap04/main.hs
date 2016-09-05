module Main where
import System.Environment
import Text.ParserCombinators.Parsec
import Control.Monad (liftM)
import Control.Monad.Error
import Data.Char
import Numeric
import Data.Ratio
import Data.Complex
import Data.Array

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
           evaled  <- return $ liftM show $ readExpr (args !! 0) >>= eval
           putStrLn $ extractValue $ trapError evaled

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
           <|> try parseVector
           <|> try parseBool
           <|> try parseCharacter
           <|> try parseComplex
           <|> try parseFloat
           <|> try parseRatio
           <|> try parseNumber
           <|> try parseQuoted
           <|> try parseQuasiQuited
           <|> try parseUnQuote
           <|> try parseList
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
                return $ Float (fst.head$readFloat (x++"."++y))

parseRatio :: Parser LispVal
parseRatio = do x <- many1 digit
                char '/'
                y <- many1 digit
                return $ Ratio((read x) % (read y))

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
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

eval:: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
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
              ("symbol?", unaryOp symbolp),
              ("string?", unaryOp stringp),
              ("number?", unaryOp numberp),
              ("bool?", unaryOp boolp),
              ("list?", unaryOp listp),
              ("symbol->string", unaryOp symbol2string),
              ("string->symbol", unaryOp string2symbol)]

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
