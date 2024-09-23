{-# LANGUAGE OverloadedStrings #-}
module Editor.UISpec (spec) where

import Test.Hspec
import qualified Data.Text as T
import Editor.UI
import Editor.MultiBuffer (initialState)
import Config.Settings (defaultSettings)
import Control.Monad.State

spec :: Spec
spec = do
  describe "UI operations" $ do
    it "renders UI without errors" $ do
      let initialEditorState = initialState "test.txt" "Hello, world!"
      (_, finalState) <- runStateT (renderUI defaultSettings) initialEditorState
      finalState `shouldBe` initialEditorState

    it "handles basic input correctly" $ do
      let initialEditorState = initialState "test.txt" "Hello, world!"
      (_, finalState) <- runStateT (handleInput defaultSettings) initialEditorState
      finalState `shouldBe` initialEditorState
