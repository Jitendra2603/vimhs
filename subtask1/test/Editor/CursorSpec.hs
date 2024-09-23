{-# LANGUAGE OverloadedStrings #-}
module Editor.CursorSpec (spec) where

import Test.Hspec
import qualified Data.Text as T
import Editor.Cursor

spec :: Spec
spec = do
  describe "Cursor operations" $ do
    it "moves left correctly" $ do
      moveLeft "Hello" (0, 3) `shouldBe` (0, 2)
      moveLeft "Hello" (0, 0) `shouldBe` (0, 0)

    it "moves right correctly" $ do
      moveRight "Hello" (0, 3) `shouldBe` (0, 4)
      moveRight "Hello" (0, 5) `shouldBe` (0, 5)

    it "moves up correctly" $ do
      moveUp (1, 3) `shouldBe` (0, 3)
      moveUp (0, 3) `shouldBe` (0, 3)

    it "moves down correctly" $ do
      moveDown 2 (0, 3) `shouldBe` (1, 3)
      moveDown 2 (2, 3) `shouldBe` (2, 3)

    it "moves to start of line correctly" $ do
      moveToStartOfLine (1, 5) `shouldBe` (1, 0)

    it "moves to end of line correctly" $ do
      moveToEndOfLine "Hello" (0, 2) `shouldBe` (0, 5)
