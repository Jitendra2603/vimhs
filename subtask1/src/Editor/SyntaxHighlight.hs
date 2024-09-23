{-# LANGUAGE OverloadedStrings #-}
module Editor.SyntaxHighlight
    ( highlightLine
    , Token(..)
    , TokenType(..)
    ) where

import qualified Data.Text as T
import Data.Char (isAlphaNum)
import Text.Regex.TDFA ((=~))

data TokenType = Keyword | Identifier | String | Number | Comment | Operator | Other deriving (Show, Eq)
data Token = Token TokenType T.Text deriving (Show, Eq)

keywords :: [T.Text]
keywords = ["let", "in", "where", "do", "case", "of", "if", "then", "else", "import", "module", "data", "type", "newtype", "class", "instance", "deriving"]

operators :: [T.Text]
operators = ["+", "-", "*", "/", "==", "/=", "<", ">", "<=", ">=", "&&", "||", "=", "->", "<-", "::", ".", "$"]

highlightLine :: T.Text -> [Token]
highlightLine = tokenize

tokenize :: T.Text -> [Token]
tokenize text
    | T.null text = []
    | T.isPrefixOf "--" text = [Token Comment text]
    | T.isPrefixOf "{-" text = let (comment, rest) = T.breakOn "-}" text
                               in Token Comment (comment <> "-}") : tokenize (T.drop 2 rest)
    | T.isPrefixOf "\"" text = let (str, rest) = T.breakOn "\"" (T.tail text)
                               in Token String (T.cons '"' $ T.snoc str '"') : tokenize (T.drop 1 rest)
    | T.isPrefixOf "'" text = let (char, rest) = T.splitAt 3 text
                              in Token String char : tokenize rest
    | isAlphaNum (T.head text) = let (word, rest) = T.span isAlphaNum text
                                 in (if word `elem` keywords then Token Keyword word else Token Identifier word) : tokenize rest
    | any (`T.isPrefixOf` text) operators = let (op, rest) = T.span (`elem` (T.concat operators)) text
                                            in Token Operator op : tokenize rest
    | T.head text `elem` ['0'..'9'] = let (num, rest) = T.span (\c -> isAlphaNum c || c == '.') text
                                      in Token Number num : tokenize rest
    | otherwise = Token Other (T.singleton (T.head text)) : tokenize (T.tail text)
