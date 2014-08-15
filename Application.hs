main :: IO ()
main = putStrLn =<< contents

contents :: IO String
contents = readFile "synonyms.txt"
