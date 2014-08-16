import System.Environment
import System.Exit

data Definition = Definition { word :: String, synonyms :: [String] }

main :: IO ()
main = do
        args <- getArgs
        contents <- readFile "synonyms.txt"
        parse args (definitions (lines contents))

parse :: [String] -> [Definition] -> IO ()
parse [] _ = do
        putStrLn "Please enter a word to lookup"
        exitFailure
parse args ds = putStr $ formatOutput $ lookupDefinition input ds where
    input = head args

formatOutput :: Maybe Definition -> String
formatOutput (Just definition) = unlines $ synonyms definition
formatOutput Nothing = "Not found"

lookupDefinition :: String -> [Definition] -> Maybe Definition
lookupDefinition _ [] = Nothing
lookupDefinition w ds = if w == word this
                            then Just this
                            else lookupDefinition w $ tail ds
                            where this = head ds

definitions :: [String] -> [Definition]
definitions = map f where
    f line = Definition w ss where
        w = head ws
        ss = tail ws
        ws = words line
