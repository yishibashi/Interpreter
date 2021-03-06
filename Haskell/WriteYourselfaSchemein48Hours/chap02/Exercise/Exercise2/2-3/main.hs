module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad


symbol :: Parser Char
symbol= oneOf "!#$%&|*+-/:<=>?@^_~"

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

escapedChars :: Parser Char
escapedChars = do char '\\'
                  x <- oneOf "\\\"nrt"
                  return $ case x of
                      '\\' -> x
                      '"' -> x
                      'n' -> '\n'
                      'r' -> '\r'
                      't' -> '\t'


parseString :: Parser LispVal
parseString = do char '"'
                 x <- many $ escapedChars <|> noneOf "\"\\"
                 char '"'
                 return $ String x

parseAtom :: Parser LispVal
parseAtom = do 
              first <- letter <|> symbol
              rest <- many (letter <|> digit <|> symbol)
              let atom = first:rest
              return $ case atom of
                        "#t" -> Bool True
                        "#f" -> Bool False
                        _    -> Atom atom


-- parseNumber :: Parser LispVal
-- parseNumber = liftM (Number . read) $ many1 digit

-- parseNumber :: Parser LispVal
-- parseNumber = do
--                x <- many1 digit
--                (return . Number . read ) x

parseNumber :: Parser LispVal
parseNumber =  many1 digit >>= return . Number . read

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber



