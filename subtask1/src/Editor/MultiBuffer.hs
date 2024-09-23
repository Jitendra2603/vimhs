{-# LANGUAGE OverloadedStrings #-}
module Editor.MultiBuffer
    ( EditorState(..)
    , emptyState
    , initialState
    , switchBuffer
    , nextBuffer
    , prevBuffer
    , getCurrentBuffer
    , getCurrentFilePath
    , updateCurrentBuffer
    , addBuffer
    , removeCurrentBuffer
    ) where

import qualified Data.Text as T
import qualified Editor.Buffer as Buffer
import Data.Sequence (Seq, (|>), ViewL(..), viewl)
import qualified Data.Sequence as Seq

data EditorState = EditorState
    { buffers :: Seq (FilePath, Buffer.Buffer)
    , currentBufferIndex :: Int
    }

emptyState :: EditorState
emptyState = EditorState Seq.empty 0

initialState :: FilePath -> T.Text -> EditorState
initialState fp content = EditorState (Seq.singleton (fp, Buffer.newBuffer content)) 0

switchBuffer :: Int -> EditorState -> EditorState
switchBuffer n state = state { currentBufferIndex = n `mod` Seq.length (buffers state) }

nextBuffer :: EditorState -> EditorState
nextBuffer state = switchBuffer (currentBufferIndex state + 1) state

prevBuffer :: EditorState -> EditorState
prevBuffer state = switchBuffer (currentBufferIndex state - 1) state

getCurrentBuffer :: EditorState -> Maybe Buffer.Buffer
getCurrentBuffer state = snd <$> Seq.index (buffers state) (currentBufferIndex state)

getCurrentFilePath :: EditorState -> Maybe FilePath
getCurrentFilePath state = fst <$> Seq.index (buffers state) (currentBufferIndex state)

updateCurrentBuffer :: (Buffer.Buffer -> Buffer.Buffer) -> EditorState -> EditorState
updateCurrentBuffer f state =
    case Seq.index (buffers state) (currentBufferIndex state) of
        Just (fp, buf) ->
            let newBuffers = Seq.update (currentBufferIndex state) (fp, f buf) (buffers state)
            in state { buffers = newBuffers }
        Nothing -> state

addBuffer :: FilePath -> T.Text -> EditorState -> EditorState
addBuffer fp content state =
    state { buffers = buffers state |> (fp, Buffer.newBuffer content)
          , currentBufferIndex = Seq.length (buffers state)
          }

removeCurrentBuffer :: EditorState -> EditorState
removeCurrentBuffer state
    | Seq.null (buffers state) = state
    | otherwise = state { buffers = Seq.deleteAt (currentBufferIndex state) (buffers state)
                        , currentBufferIndex = max 0 (currentBufferIndex state - 1)
                        }
