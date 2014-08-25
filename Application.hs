import System.Environment
import System.Exit

type Word = String
type Synonyms = [String]

data Definition = Definition { word :: Word, synonyms :: Synonyms }

main :: IO ()
main = do
        args <- getArgs
        contents <- readFile "synonyms.txt"
        parse args $ definitions (lines contents)

parse :: [Word] -> [Definition] -> IO ()
parse [] _ = do
        putStrLn "Please enter a word to lookup"
        exitFailure
parse args ds = putStr . formatOutput $ lookupDefinition input ds where
    input = head args

formatOutput :: Maybe Definition -> String
formatOutput (Just definition) = unlines $ synonyms definition
formatOutput Nothing = "Not found"

lookupDefinition :: Word -> [Definition] -> Maybe Definition
lookupDefinition _ [] = Nothing
lookupDefinition w ds = if w == word this
                            then Just this
                            else lookupDefinition w $ tail ds
                            where this = head ds

definitions :: [String] -> [Definition]
definitions = map convertToDefinition

convertToDefinition :: String -> Definition
convertToDefinition line = Definition (head ws) (tail ws) where
    ws = words line
