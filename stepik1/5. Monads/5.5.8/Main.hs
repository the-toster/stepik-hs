module Main where
import System.IO
import System.Directory (getDirectoryContents, removeFile)
import Data.List (isInfixOf)

main = main'
main' :: IO ()
main' = do
    putStr   "Substring: "
    hFlush stdout
    name <- getLine
    if null name then do
        putStrLn "Canceled"
        return ()
        else do
        files <- getDirectoryContents "."
        let toRemove = filter (isInfixOf name) files
        mapM_ removeFileAndReport toRemove

removeFileAndReport fileName = do
    removeFile fileName
    putStrLn $ "Removing file: " ++ fileName
