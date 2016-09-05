module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Numeric
import Data.Ratio
import Data.Complex

symbol :: Parser Char
symbol= oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

main :: IO ()
main = do 
          args <- getArgs
          putStrLn (readExpr (args !! 0))


data LispVal = Atom String
              |List [LispVal]
              |DottedList [LispVal] LispVal
              |Number Integer
              |String String
              |Bool Bool
              |Character Char
              |Float Double
              |Ratio Rational
              |Complex (Complex Double)
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



parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> try parseBool
        <|> try parseCharacter
        <|> try parseComplex
        <|> try parseFloat
        <|> try parseRatio
        <|> try parseNumber



