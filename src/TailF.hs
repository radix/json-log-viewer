{-# LANGUAGE OverloadedStrings #-}

module TailF where

import           Control.Concurrent   (threadDelay)
import           Control.Exception    (tryJust)
import           Control.Monad        (forever, guard)
import qualified Data.ByteString.Lazy as BS
import           System.Environment   (getArgs)
import           System.Exit          (exitFailure, exitSuccess)
import           System.IO            (IOMode (ReadMode),
                                       SeekMode (AbsoluteSeek), hPutStrLn,
                                       hSeek, openFile, stderr)
import           System.IO.Error      (isDoesNotExistError)
import           System.Posix.Files   (fileSize, getFileStatus)

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
  -> (Either String [BS.ByteString] -> IO ()) -- ^ function to be called with
                                              -- the new complete lines
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
           callback $ Right $ init lines
           go startNext
         else
           go sizeSoFar
