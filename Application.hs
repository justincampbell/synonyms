import System.Environment
import System.Exit

data Definition = Definition { word :: String, synonyms :: [String] }

main :: IO ()
main = do
        args <- getArgs
        parse args

parse :: [String] -> IO ()
parse [] = do
        putStrLn "Please enter a word to lookup"
        exitFailure
parse args = putStrLn $ formatOutput $ lookupDefinition input definitions where
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

definitions :: [Definition]
definitions = [Definition "foo" ["bar", "baz"]]
