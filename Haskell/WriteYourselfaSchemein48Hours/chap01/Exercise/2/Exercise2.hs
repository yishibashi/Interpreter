module Main where
import System.Environment

main :: IO ()
main = do
    nums <- getArgs
    let n1 = read(nums !! 0) 
    let n2 = read(nums !! 1)

    putStrLn("+: " ++ show(n1 + n2))
    putStrLn("-: " ++ show(n1 - n2))
    putStrLn("*: " ++ show(n1 * n2))
    putStrLn("/: " ++ show(n1 / n2))


