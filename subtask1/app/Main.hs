{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Editor.UI as UI
import qualified Editor.Buffer as Buffer
import qualified Editor.FileIO as FileIO
import qualified Editor.MultiBuffer as MultiBuffer
import qualified Config.Settings as Settings
import Control.Monad.State
import System.Environment (getArgs)
import Control.Exception (catch, SomeException)
import System.IO (hPutStrLn, stderr)
import Data.Text (Text)
import qualified Data.Text as T
import System.Exit (exitFailure)

main :: IO ()
main = do
    args <- getArgs
    settings <- Settings.loadSettings `catch` handleConfigError
    initialState <- case args of
        [] -> return MultiBuffer.emptyState
        (file:_) -> loadFile file `catch` handleFileError
    evalStateT (UI.runEditor settings) initialState
  where
    loadFile :: FilePath -> IO MultiBuffer.EditorState
    loadFile file = do
        content <- FileIO.readFile file
        return $ MultiBuffer.initialState file content

    handleConfigError :: SomeException -> IO Settings.Settings
    handleConfigError e = do
        hPutStrLn stderr $ "Error loading config: " ++ show e
        hPutStrLn stderr "Using default settings"
        return Settings.defaultSettings

    handleFileError :: SomeException -> IO MultiBuffer.EditorState
    handleFileError e = do
        hPutStrLn stderr $ "Error loading file: " ++ show e
        exitFailure
