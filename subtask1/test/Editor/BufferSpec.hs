{-# LANGUAGE OverloadedStrings #-}
module Editor.BufferSpec (spec) where

import Test.Hspec
import qualified Data.Text as T
import Editor.Buffer

spec :: Spec
spec = do
  describe "Buffer operations" $ do
    it "creates a new buffer correctly" $ do
      let buf = newBuffer "Hello, world!"
      getContent buf `shouldBe` "Hello, world!"
      getCursor buf `shouldBe` (0, 0)

    it "inserts a character correctly" $ do
      let buf = newBuffer "Hello"
          buf' = insertChar '!' (moveCursor (0, 5) buf)
      getContent buf' `shouldBe` "Hello!"
      getCursor buf' `shouldBe` (0, 6)

    it "deletes a character correctly" $ do
      let buf = newBuffer "Hello"
          buf' = deleteChar (moveCursor (0, 4) buf)
      getContent buf' `shouldBe` "Hell"
      getCursor buf' `shouldBe` (0, 3)

    it "moves cursor correctly" $ do
      let buf = newBuffer "Hello\nWorld"
          buf' = moveCursor (1, 2) buf
      getCursor buf' `shouldBe` (1, 2)

    it "replaces range correctly" $ do
      let buf = newBuffer "Hello, world!"
          buf' = replaceRange (0, 0) (0, 5) "Hi" buf
      getContent buf' `shouldBe` "Hi, world!"
      getCursor buf' `shouldBe` (0, 2)

    it "inserts line correctly" $ do
      let buf = newBuffer "Hello\nWorld"
          buf' = insertLine 1 "New line" buf
      getContent buf' `shouldBe` "Hello\nNew line\nWorld"
      getCursor buf' `shouldBe` (2, 0)

    it "deletes line correctly" $ do
      let buf = newBuffer "Hello\nWorld\nGoodbye"
          buf' = deleteLine 1 buf
      getContent buf' `shouldBe` "Hello\nGoodbye"
      getCursor buf' `shouldBe` (0, 0)

    it "joins lines correctly" $ do
      let buf = newBuffer "Hello\nWorld"
          buf' = joinLine 0 buf
      getContent buf' `shouldBe` "HelloWorld"
      getCursor buf' `shouldBe` (0, 5)

    it "undoes and redoes operations correctly" $ do
      let buf = newBuffer "Hello"
          buf' = insertChar '!' buf
          buf'' = undo buf'
          buf''' = redo buf''
      getContent buf'' `shouldBe` "Hello"
      getContent buf''' `shouldBe` "Hello!"
