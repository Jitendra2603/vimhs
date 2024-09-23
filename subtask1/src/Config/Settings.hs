{-# LANGUAGE OverloadedStrings #-}
module Config.Settings
    ( Settings(..)
    , loadSettings
    , defaultSettings
    ) where

import qualified Data.Yaml as Y
import qualified Data.ByteString as BS
import Data.Text (Text)
import System.Console.ANSI (Color(..))

data Settings = Settings
    { tabWidth :: Int
    , lineNumbers :: Bool
    , theme :: Text
    , keywordColor :: Color
    , identifierColor :: Color
    , stringColor :: Color
    , numberColor :: Color
    , commentColor :: Color
    , operatorColor :: Color
    , defaultColor :: Color
    , statusLineColor :: Color
    } deriving (Show, Eq)

instance Y.FromJSON Settings where
    parseJSON = Y
