module Main where

import qualified Editor.UI as UI
import qualified Editor.Buffer as Buffer
import qualified Editor.FileIO as FileIO
import qualified Editor.MultiBuffer as MultiBuffer
import qualified Config.Settings as Settings
import Control.Monad.State
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    initialState <- case args of
        [] -> return $ MultiBuffer.emptyState
        (file:_) -> do
            content <- FileIO.readFile file
            return $ MultiBuffer.initialState file content
    settings <- Settings.loadSettings
    evalStateT (UI.runEditor settings) initialState
