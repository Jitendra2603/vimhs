{-# LANGUAGE OverloadedStrings #-}

module Editor.Buffer
    ( Buffer(..)
    , newBuffer
    , insertChar
    , deleteChar
    , insertLine
    , deleteLine
    , moveCursor
    , getCursorPosition
    , getLines
    , replaceLines
    , undo
    , redo
    ) where

import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Sequence (Seq, (|>), ViewL(..), (<|))
import qualified Data.Sequence as Seq

data Buffer = Buffer
    { bufferLines :: V.Vector T.Text
    , cursorRow :: !Int
    , cursorCol :: !Int
    , undoStack :: Seq Buffer
    , redoStack :: Seq Buffer
    }

newBuffer :: T.Text -> Buffer
newBuffer content =
    Buffer (V.fromList $ T.lines content) 0 0 Seq.empty Seq.empty

insertChar :: Char -> Buffer -> Buffer
insertChar c buf@(Buffer lines row col undoS redoS) =
    let (before, after) = T.splitAt col (lines V.! row)
        newLine = before `T.snoc` c `T.append` after
        newLines = lines V.// [(row, newLine)]
    in pushUndo $ buf { bufferLines = newLines, cursorCol = col + 1, redoStack = Seq.empty }

deleteChar :: Buffer -> Buffer
deleteChar buf@(Buffer lines row col undoS redoS)
    | col > 0 =
        let (before, after) = T.splitAt (col - 1) (lines V.! row)
            newLine = before `T.append` after
            newLines = lines V.// [(row, newLine)]
        in pushUndo $ buf { bufferLines = newLines, cursorCol = col - 1, redoStack = Seq.empty }
    | row > 0 =
        let prevLine = lines V.! (row - 1)
            currLine = lines V.! row
            newLine = prevLine `T.append` currLine
            newLines = V.take (row - 1) lines V.++ V.singleton newLine V.++ V.drop (row + 1) lines
        in pushUndo $ buf { bufferLines = newLines, cursorRow = row - 1, cursorCol = T.length prevLine, redoStack = Seq.empty }
    | otherwise = buf

insertLine :: Buffer -> Buffer
insertLine buf@(Buffer lines row col undoS redoS) =
    let (before, after) = T.splitAt col (lines V.! row)
        newLines = V.take row lines
               V.++ V.fromList [before, after]
               V.++ V.drop (row + 1) lines
    in pushUndo $ buf { bufferLines = newLines, cursorRow = row + 1, cursorCol = 0, redoStack = Seq.empty }

deleteLine :: Buffer -> Buffer
deleteLine buf@(Buffer lines row col undoS redoS)
    | V.length lines > 1 =
        let newLines = V.take row lines V.++ V.drop (row + 1) lines
            newRow = if row >= V.length newLines then V.length newLines - 1 else row
            newCol = min col (T.length (newLines V.! newRow) - 1)
        in pushUndo $ buf { bufferLines = newLines, cursorRow = newRow, cursorCol = newCol, redoStack = Seq.empty }
    | otherwise = buf

moveCursor :: (Int -> Int -> (Int, Int)) -> Buffer -> Buffer
moveCursor f buf@(Buffer lines row col _ _) =
    let (newRow, newCol) = f row col
        maxRow = V.length lines - 1
        maxCol = T.length (lines V.! newRow)
    in buf { cursorRow = max 0 $ min maxRow newRow
           , cursorCol = max 0 $ min maxCol newCol
           }

getCursorPosition :: Buffer -> (Int, Int)
getCursorPosition (Buffer _ row col _ _) = (row, col)

getLines :: Buffer -> [T.Text]
getLines (Buffer lines _ _ _ _) = V.toList lines

replaceLines :: [T.Text] -> Buffer -> Buffer
replaceLines newLines buf =
    pushUndo $ buf { bufferLines = V.fromList newLines, cursorRow = 0, cursorCol = 0, redoStack = Seq.empty }

pushUndo :: Buffer -> Buffer
pushUndo buf@(Buffer _ _ _ undoS _) =
    buf { undoStack = undoS |> buf }

undo :: Buffer -> Buffer
undo buf@(Buffer _ _ _ undoS redoS) =
    case Seq.viewr undoS of
        Seq.EmptyR -> buf
        rest Seq.:> prev -> prev { undoStack = rest, redoStack = buf <| redoS }

redo :: Buffer -> Buffer
redo buf@(Buffer _ _ _ undoS redoS) =
    case Seq.viewl redoS of
        EmptyL -> buf
        next :< rest -> next { undoStack = undoS |> buf, redoStack = rest }
