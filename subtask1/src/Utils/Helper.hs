module Utils.Helpers
    ( safeHead
    , safeTail
    , safeInit
    , safeLast
    , splitAtLine
    , joinLines
    , replaceSubstring
    ) where

import qualified Data.Text as T

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:xs) = Just xs

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit [_] = Just []
safeInit (x:xs) = Just (x : maybe [] id (safeInit xs))

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast [x] = Just x
safeLast (_:xs) = safeLast xs

splitAtLine :: Int -> T.Text -> (T.Text, T.Text)
splitAtLine n text =
    let lines = T.lines text
        (before, after) = splitAt n lines
    in (T.unlines before, T.unlines after)

joinLines :: [T.Text] -> T.Text
joinLines = T.intercalate "\n"

replaceSubstring :: T.Text -> T.Text -> T.Text -> T.Text
replaceSubstring old new = T.intercalate new . T.splitOn old
