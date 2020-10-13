module Main where
import System.IO

main = main'
main' :: IO ()
main' = do
    putStrLn "What is your name?"
    putStr   "Name: "
    hFlush stdout
    name <- getLine
    if null name then main' else putStrLn $ "Hi, " ++ name ++ "!"
