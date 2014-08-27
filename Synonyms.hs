module Synonyms where

type Word = String
type Synonyms = [String]

data Definition = Definition {
                word :: Word,
                synonyms :: Synonyms
                } deriving (Eq, Show)

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
