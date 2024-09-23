{-# LANGUAGE OverloadedStrings #-}
module Editor.FileIO
    ( readFile
    , writeFile
    , detectEncoding
    , Encoding(..)
    , autoSave
    , watchFileChanges
    ) where

import Prelude hiding (readFile, writeFile)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import System.IO (IOMode(..), withFile, hSetEncoding, utf8)
import Control.Exception (catch, SomeException, throwIO)
import Control.Concurrent (forkIO, threadDelay, MVar, newMVar, modifyMVar_, readMVar)
import Control.Monad (forever, when)
import System.Directory (getModificationTime)
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
import System.FSNotify

data Encoding = UTF8 | UTF16LE | UTF16BE | ASCII deriving (Show, Eq)

detectEncoding :: FilePath -> IO Encoding
detectEncoding path = do
    bytes <- BS.readFile path
    return $ case BS.take 2 bytes of
        "\xFE\xFF" -> UTF16BE
        "\xFF\xFE" -> UTF16LE
        _ -> if BS.all (<= 0x7F) bytes then ASCII else UTF8

readFile :: FilePath -> IO T.Text
readFile path = do
    encoding <- detectEncoding path
    case encoding of
        UTF8 -> TIO.readFile path
        UTF16LE -> fmap (TE.decodeUtf16LE . BS.drop 2) (BS.readFile path)
        UTF16BE -> fmap (TE.decodeUtf16BE . BS.drop 2) (BS.readFile path)
        ASCII -> fmap TE.decodeASCII (BS.readFile path)

writeFile :: FilePath -> T.Text -> IO ()
writeFile path content = do
    encoding <- detectEncoding path
    case encoding of
        UTF8 -> TIO.writeFile path content
        UTF16LE -> BS.writeFile path $ "\xFF\xFE" <> TE.encodeUtf16LE content
        UTF16BE -> BS.writeFile path $ "\xFE\xFF" <> TE.encodeUtf16BE content
        ASCII -> BS.writeFile path $ TE.encodeASCII content

autoSave :: FilePath -> IO T.Text -> MVar UTCTime -> IO ()
autoSave path getContent lastSaveMVar = forever $ do
    threadDelay (5 * 1000000)  -- 5 seconds
    currentTime <- getCurrentTime
    lastSaveTime <- readMVar lastSaveMVar
    when (diffUTCTime currentTime lastSaveTime > 300) $ do  -- 5 minutes
        content <- getContent
        writeFile path content
        modifyMVar_ lastSaveMVar (const $ return currentTime)

watchFileChanges :: FilePath -> (T.Text -> IO ()) -> IO ()
watchFileChanges path updateBufferCallback = do
    manager <- startManager
    _ <- watchTree manager (T.pack $ takeDirectory path) (const True) $ \event ->
        case event of
            Modified {} -> do
                newContent <- readFile path
                updateBufferCallback newContent
            _ -> return ()
    forever $ threadDelay 1000000

initializeFileIO :: FilePath -> IO T.Text -> (T.Text -> IO ()) -> IO ()
initializeFileIO path getContent updateBufferCallback = do
    lastSaveMVar <- newMVar =<< getCurrentTime
    _ <- forkIO $ autoSave path getContent lastSaveMVar
    _ <- forkIO $ watchFileChanges path updateBufferCallback
    return ()
