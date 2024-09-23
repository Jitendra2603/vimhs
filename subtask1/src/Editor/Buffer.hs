{-# LANGUAGE OverloadedStrings #-}
module Editor.Buffer
    ( Buffer
    , newBuffer
    , insertChar
    , deleteChar
    , moveCursor
    , getLine
    , getContent
    , getCursor
    , replaceRange
    , insertLine
    , deleteLine
    , joinLine
    , undo
    , redo
    ) where

import qualified Data.Text as T
import qualified Data.Text.Zipper as Z
import Editor.Cursor (Cursor)
import Data.Sequence (Seq, (|>), ViewL(..), viewl)
import qualified Data.Sequence as Seq

data Buffer = Buffer
    { bufferContent :: Z.TextZipper
    , bufferCursor :: Cursor
    , undoStack :: Seq (Z.TextZipper, Cursor)
    , redoStack :: Seq (Z.TextZipper, Cursor)
    }

newBuffer :: T.Text -> Buffer
newBuffer content = Buffer
    { bufferContent = Z.fromText content
    , bufferCursor = (0, 0)
    , undoStack = Seq.empty
    , redoStack = Seq.empty
    }

saveState :: Buffer -> Buffer
saveState buf = buf
    { undoStack = undoStack buf |> (bufferContent buf, bufferCursor buf)
    , redoStack = Seq.empty
    }

insertChar :: Char -> Buffer -> Buffer
insertChar c buf = saveState $ buf
    { bufferContent = Z.insertChar c (bufferContent buf)
    , bufferCursor = (fst (bufferCursor buf), snd (bufferCursor buf) + 1)
    }

deleteChar :: Buffer -> Buffer
deleteChar buf = saveState $ buf
    { bufferContent = Z.deleteChar (bufferContent buf)
    , bufferCursor = (fst (bufferCursor buf), max 0 (snd (bufferCursor buf) - 1))
    }

moveCursor :: (Int, Int) -> Buffer -> Buffer
moveCursor (row, col) buf = buf
    { bufferContent = Z.moveCursor row col (bufferContent buf)
    , bufferCursor = (row, col)
    }

getLine :: Int -> Buffer -> T.Text
getLine n buf = Z.currentLine $ Z.moveCursor n 0 (bufferContent buf)

getContent :: Buffer -> T.Text
getContent = Z.getText . bufferContent

getCursor :: Buffer -> Cursor
getCursor = bufferCursor

replaceRange :: (Int, Int) -> (Int, Int) -> T.Text -> Buffer -> Buffer
replaceRange (startRow, startCol) (endRow, endCol) newText buf = saveState $ buf
    { bufferContent = Z.replaceRange startRow startCol endRow endCol newText (bufferContent buf)
    , bufferCursor = (startRow, startCol + T.length newText)
    }

insertLine :: Int -> T.Text -> Buffer -> Buffer
insertLine row text buf = saveState $ buf
    { bufferContent = Z.insertLine row text (bufferContent buf)
    , bufferCursor = (row + 1, 0)
    }

deleteLine :: Int -> Buffer -> Buffer
deleteLine row buf = saveState $ buf
    { bufferContent = Z.deleteLine row (bufferContent buf)
    , bufferCursor = (max 0 (row - 1), 0)
    }

joinLine :: Int -> Buffer -> Buffer
joinLine row buf = saveState $ buf
    { bufferContent = Z.joinLine row (bufferContent buf)
    , bufferCursor = (row, T.length $ getLine row buf)
    }

undo :: Buffer -> Buffer
undo buf = case viewl (undoStack buf) of
    EmptyL -> buf
    (content, cursor) :< rest -> buf
        { bufferContent = content
        , bufferCursor = cursor
        , undoStack = rest
        , redoStack = (bufferContent buf, bufferCursor buf) |> redoStack buf
        }

redo :: Buffer -> Buffer
redo buf = case viewl (redoStack buf) of
    EmptyL -> buf
    (content, cursor) :< rest -> buf
        { bufferContent = content
        , bufferCursor = cursor
        , redoStack = rest
        , undoStack = (bufferContent buf, bufferCursor buf) |> undoStack buf
        }
