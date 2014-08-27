module Main where

import Synonyms
import Test.Hspec

main :: IO ()
main = hspec $ do
    let d = Definition "foo" ["b"] in
        describe "lookupDefinition" $ do
            it "returns Nothing when no matches" $
                lookupDefinition "a" [d] `shouldBe` Nothing

            it "returns Just Definition when found" $
                lookupDefinition "foo" [d] `shouldBe` Just d

    describe "definitions" $
        it "converts a list of definitions" $
            definitions ["foo bar baz"] `shouldBe` [Definition "foo" ["bar", "baz"]]

    describe "convertToDefinition" $
        it "converts to a definition" $
            convertToDefinition "foo bar baz" `shouldBe` Definition "foo" ["bar", "baz"]
