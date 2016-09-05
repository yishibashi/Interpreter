module Main where
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    putStrLn ("args1: " ++ args !! 0 ++ " args2: " ++ args !! 1)

