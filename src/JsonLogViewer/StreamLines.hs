module JsonLogViewer.StreamLines (LinesCallback(..), streamLines) where

import           Control.Monad   (forever)
import qualified Data.ByteString as BS
import           System.IO       (BufferMode (LineBuffering), Handle,
                                  hIsSeekable, hReady, hSetBuffering)


newtype LinesCallback = LinesCallback { unLinesCallback :: [BS.ByteString] -> IO () }

-- |Given a Handle, call the callback function
streamLines :: Handle -> LinesCallback -> IO ()
streamLines handle callback = do
  isNormalFile <- hIsSeekable handle
  if isNormalFile then cb =<< BS.split 10 <$> BS.hGetContents handle
  else do
    -- it's a real stream, so let's stream it
    hSetBuffering handle LineBuffering
    forever $ do
      availableLines <- hGetLines handle
      if not $ null availableLines then cb availableLines
      else do
        -- fall back to a blocking read now that we've reached the end of the
        -- stream
        line <- BS.hGetLine handle
        cb [line]
  where cb = unLinesCallback callback

-- |Get all the lines available from a handle, *hopefully* without blocking
-- This will sadly still block if there is a partial line at the end of the
-- stream and no more data is being written. hGetBufNonBlocking would allow me
-- to work around this problem, but then I'd have to keep my own buffer!
hGetLines :: Handle -> IO [BS.ByteString]
hGetLines handle = do
  readable <- hReady handle
  if readable then (:) <$> BS.hGetLine handle <*> hGetLines handle
  else return []
