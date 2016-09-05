module Main where
import System.Environment

main :: IO ()
main = do
    putStrLn ("Input your name.")
    name <- getLine
    putStrLn ("Hello, " ++ name)
