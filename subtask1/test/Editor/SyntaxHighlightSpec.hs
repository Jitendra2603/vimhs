{-# LANGUAGE OverloadedStrings #-}
module Editor.SyntaxHighlightSpec (spec) where

import Test.Hspec
import qualified Data.Text as T
import Editor.SyntaxHighlight

spec :: Spec
spec = do
  describe "Syntax highlighting" $ do
    it "highlights keywords correctly" $ do
      let tokens = highlightLine "let x = 5 in x"
      tokens `shouldContain` [Token Keyword "let"]
      tokens `shouldContain` [Token Keyword "in"]

    it "highlights strings correctly" $ do
      let tokens = highlightLine "x = \"Hello, world!\""
      tokens `shouldContain` [Token String "\"Hello, world!\""]

    it "highlights numbers correctly" $ do
      let tokens = highlightLine "x = 42"
      tokens `shouldContain` [Token Number "42"]

    it "highlights comments correctly" $ do
      let tokens = highlightLine "-- This is a comment"
      tokens `shouldBe` [Token Comment "-- This is a comment"]

    it "highlights operators correctly" $ do
      let tokens = highlightLine "x + y * z"
      tokens `shouldContain` [Token Operator "+"]
      tokens `shouldContain` [Token Operator "*"]
