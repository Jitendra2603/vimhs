{-# LANGUAGE OverloadedStrings #-}
module Editor.UI
    ( runEditor
    ) where

import qualified Data.Text as T
import qualified Editor.Buffer as Buffer
import qualified Editor.MultiBuffer as MB
import qualified Editor.SyntaxHighlight as SH
import qualified Config.Settings as Settings
import System.Console.ANSI
import Control.Monad.State
import Control.Monad.IO.Class (liftIO)
import System.IO (hFlush, stdout)
import Data.Maybe (fromMaybe)

runEditor :: Settings.Settings -> StateT MB.EditorState IO ()
runEditor settings = do
    liftIO $ clearScreen
    forever $ do
        renderUI settings
        handleInput settings

renderUI :: Settings.Settings -> StateT MB.EditorState IO ()
renderUI settings = do
    state <- get
    case MB.getCurrentBuffer state of
        Just buf -> do
            liftIO $ setCursorPosition 0 0
            let content = Buffer.getContent buf
                (cursorRow, cursorCol) = Buffer.getCursor buf
            liftIO $ mapM_ (putStrLn . T.unpack . highlightLine settings) (T.lines content)
            liftIO $ renderStatusLine settings state
            liftIO $ setCursorPosition cursorRow cursorCol
            liftIO $ hFlush stdout
        Nothing -> liftIO $ putStrLn "No buffer open"

highlightLine :: Settings.Settings -> T.Text -> T.Text
highlightLine settings line = T.concat $ map (tokenToColoredText settings) (SH.highlightLine line)

tokenToColoredText :: Settings.Settings -> SH.Token -> T.Text
tokenToColoredText settings (SH.Token tokenType text) =
    let color = case tokenType of
            SH.Keyword -> Settings.keywordColor settings
            SH.Identifier -> Settings.identifierColor settings
            SH.String -> Settings.stringColor settings
            SH.Number -> Settings.numberColor settings
            SH.Comment -> Settings.commentColor settings
            SH.Operator -> Settings.operatorColor settings
            SH.Other -> Settings.defaultColor settings
    in T.pack (setSGRCode [SetColor Foreground Vivid color]) <> text <> T.pack (setSGRCode [Reset])

renderStatusLine :: Settings.Settings -> MB.EditorState -> IO ()
renderStatusLine settings state = do
    (height, width) <- getTerminalSize
    setCursorPosition (height - 1) 0
    setSGR [SetColor Foreground Vivid (Settings.statusLineColor settings), SetColor Background Dull Blue]
    let statusText = case MB.getCurrentFilePath state of
            Just fp -> T.pack fp <> " | " <> T.pack (show (MB.currentBufferIndex state + 1)) <> "/" <> T.pack (show (length (MB.buffers state)))
            Nothing -> "No file"
    putStr $ T.unpack $ T.justifyLeft width ' ' statusText
    setSGR [Reset]

handleInput :: Settings.Settings -> StateT MB.EditorState IO ()
handleInput settings = do
    c <- liftIO getChar
    case c of
        'q' -> liftIO exitSuccess
        'h' -> modify $ MB.updateCurrentBuffer (Buffer.moveCursor (-1, 0))
        'j' -> modify $ MB.updateCurrentBuffer (Buffer.moveCursor (0, 1))
        'k' -> modify $ MB.updateCurrentBuffer (Buffer.moveCursor (0, -1))
        'l' -> modify $ MB.updateCurrentBuffer (Buffer.moveCursor (1, 0))
        'n' -> modify MB.nextBuffer
        'p' -> modify MB.prevBuffer
        'i' -> enterInsertMode settings
        ':' -> handleCommand settings
        _ -> return ()

enterInsertMode :: Settings.Settings -> StateT MB.EditorState IO ()
enterInsertMode settings = do
    liftIO $ putStr "-- INSERT --"
    liftIO $ hFlush stdout
    insertLoop
  where
    insertLoop = do
        c <- liftIO getChar
        case c of
            '\ESC' -> return ()
            _ -> do
                modify $ MB.updateCurrentBuffer (Buffer.insertChar c)
                insertLoop

handleCommand :: Settings.Settings -> StateT MB.EditorState IO ()
handleCommand settings = do
    liftIO $ putStr ":"
    liftIO $ hFlush stdout
    cmd <- liftIO getLine
    case words cmd of
        ["w"] -> saveCurrentBuffer
        ["q"] -> liftIO exitSuccess
        ["wq"] -> saveCurrentBuffer >> liftIO exitSuccess
        ["e", file] -> openFile file
        _ -> liftIO $ putStrLn "Unknown command"

saveCurrentBuffer :: StateT MB.EditorState IO ()
saveCurrentBuffer = do
    state <- get
    case (MB.getCurrentFilePath state, MB.getCurrentBuffer state) of
        (Just fp, Just buf) -> liftIO $ do
            let content = Buffer.getContent buf
            writeFile fp (T.unpack content)
            putStrLn $ "Saved " ++ fp
        _ -> liftIO $ putStrLn "No file to save"

openFile :: FilePath -> StateT MB.EditorState IO ()
openFile file = do
    content <- liftIO $ readFile file
    modify $ MB.addBuffer file (T.pack content)

getTerminalSize :: IO (Int, Int)
getTerminalSize = do
    size <- getTerminalSize
    return $ fromMaybe (24, 80) size
