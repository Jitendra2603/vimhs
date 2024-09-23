{-# LANGUAGE OverloadedStrings #-}
module Editor.MultiBufferSpec (spec) where

import Test.Hspec
import qualified Data.Text as T
import Editor.MultiBuffer
import Editor.Buffer (getContent)

spec :: Spec
spec = do
  describe "MultiBuffer operations" $ do
    it "creates an empty state correctly" $ do
      let state = emptyState
      buffers state `shouldBe` mempty
      currentBufferIndex state `shouldBe` 0

    it "initializes with a single buffer correctly" $ do
      let state = initialState "test.txt" "Hello, world!"
      length (buffers state) `shouldBe` 1
      currentBufferIndex state `shouldBe` 0

    it "switches buffers correctly" $ do
      let state = initialState "test1.txt" "Buffer 1"
          state' = addBuffer "test2.txt" "Buffer 2" state
          state'' = switchBuffer 1 state'
      currentBufferIndex state'' `shouldBe` 1

    it "cycles through buffers correctly" $ do
      let state = initialState "test1.txt" "Buffer 1"
          state' = addBuffer "test2.txt" "Buffer 2" state
          state'' = nextBuffer state'
          state''' = nextBuffer state''
      currentBufferIndex state''' `shouldBe` 0

    it "updates current buffer correctly" $ do
      let state = initialState "test.txt" "Hello"
          state' = updateCurrentBuffer (Buffer.insertChar '!') state
      case getCurrentBuffer state' of
        Just buf -> getContent buf `shouldBe` "Hello!"
        Nothing -> expectationFailure "Buffer should exist"

    it "removes current buffer correctly" $ do
      let state = initialState "test1.txt" "Buffer 1"
          state' = addBuffer "test2.txt" "Buffer 2" state
          state'' = removeCurrentBuffer state'
      length (buffers state'') `shouldBe` 1
      currentBufferIndex state'' `shouldBe` 0
