#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent      (threadDelay)
import           Control.Concurrent.MVar (MVar (..), putMVar, takeMVar)
import           Control.Exception       (tryJust)
import           Control.Monad           (forever, guard)
import qualified Data.ByteString.Lazy    as BS
import           System.Environment      (getArgs)
import           System.Exit             (exitFailure, exitSuccess)
import           System.IO               (IOMode (ReadMode),
                                          SeekMode (AbsoluteSeek), hPutStrLn,
                                          hSeek, openFile, stderr)
import           System.IO.Error         (isDoesNotExistError)
import           System.Posix.Files      (fileSize, getFileStatus)


-- some type aliases for readability

type FilePosition = Integer
type MicroSeconds = Int

-- TODO: Implement `tail -F` behavior -- streamLines is `tail -f`

-- |Tail a file, sending complete lines to the passed-in IO function.
-- If the file disappears, streamLines will return, and the function will be
-- called one final time with a Left.
streamLines
  :: FilePath
  -> FilePosition -- ^ Position in the file to start reading at. Very likely
                  -- you want to pass 0.
  -> MicroSeconds -- ^ delay between each check of the file in microseconds
  -> (Either String BS.ByteString -> IO ()) -- ^ function to be called with
                                            -- each new complete line
  -> IO ()
streamLines path sizeSoFar delay callback = go sizeSoFar
  where
    go sizeSoFar = do
      threadDelay delay
      errorOrStat <- tryJust (guard . isDoesNotExistError) $ getFileStatus path
      case errorOrStat of
       Left e -> callback $ Left "File does not exist"
       Right stat -> do
         let newSize = fromIntegral $ fileSize stat :: Integer
         if newSize > sizeSoFar
         then do
           handle <- openFile path ReadMode
           hSeek handle AbsoluteSeek sizeSoFar
           newContents <- BS.hGetContents handle
           let lines = BS.splitWith (==10) newContents
           let startNext = newSize - (toInteger $ BS.length $ last lines)
           mapM_ (callback . Right) $ init lines
           go startNext
         else do
           go sizeSoFar

usage = "usage: tailf file"

tailCallback :: Either String BS.ByteString -> IO ()
tailCallback (Left e) = do
  hPutStrLn stderr "aborting: file does not exist"
  exitFailure
tailCallback (Right line) = do
  BS.putStr line
  BS.putStr $ BS.singleton 10

main = do
    args <- getArgs
    case args of
        [] -> do
            putStrLn usage
            exitFailure
        ["-h"] -> do
            putStrLn usage
            exitSuccess
        ["--help"] -> do
            putStrLn usage
            exitSuccess
        [path] -> do
            streamLines path 0 1000000 tailCallback
