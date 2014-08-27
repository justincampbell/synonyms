import Synonyms
import System.Environment
import System.Exit

main :: IO ()
main = do
        args <- getArgs
        contents <- readFile "synonyms.txt"
        parse args $ definitions (lines contents)

parse :: [Word] -> [Definition] -> IO ()
parse [] _ = do
        putStrLn "Please enter a word to lookup"
        exitFailure
parse args ds = putStr $ formatOutput $ lookupDefinition input ds where
    input = head args

formatOutput :: Maybe Definition -> String
formatOutput (Just definition) = unlines $ synonyms definition
formatOutput Nothing = "Not found"
