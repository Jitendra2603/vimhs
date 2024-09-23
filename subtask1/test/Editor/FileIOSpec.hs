{-# LANGUAGE OverloadedStrings #-}
module Editor.FileIOSpec (spec) where

import Test.Hspec
import qualified Data.Text as T
import Editor.FileIO
import System.IO.Temp (withSystemTempFile)
import Control.Exception (bracket)

spec :: Spec
spec = do
  describe "FileIO operations" $ do
    it "detects encoding correctly" $ do
      withTestFile "Hello" $ \path -> do
        encoding <- detectEncoding path
        encoding `shouldBe` UTF8

    it "reads and writes files correctly" $ do
      withTestFile "Hello, world!" $ \path -> do
        content <- readFile path
        content `shouldBe` "Hello, world!"
        writeFile path "New content"
        newContent <- readFile path
        newContent `shouldBe` "New content"

    it "handles different encodings" $ do
      let testContent = "Hello, 世界!"
      withTestFile testContent $ \path -> do
        content <- readFile path
        content `shouldBe` testContent

withTestFile :: T.Text -> (FilePath -> IO a) -> IO a
withTestFile content action =
  withSystemTempFile "test.txt" $ \path handle -> do
    T.hPutStr handle content
    action path
