{-# LANGUAGE NamedWildCards        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections         #-}

module Main where


import           Control.Concurrent         (forkIO)
import           Control.Monad              (unless, when)
import qualified Data.Aeson                 as Aeson
import           Data.Aeson.Encode.Pretty   (encodePretty)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as BSL
import           Data.Char                  (isSpace)
import Data.Default (def)
import           Data.Foldable              (toList)
import qualified Data.HashMap.Strict        as HM
import           Data.IORef                 (IORef, modifyIORef, newIORef,
                                             readIORef, writeIORef)
import           Data.Maybe                 (mapMaybe)
import           Data.Sequence              ((><))
import qualified Data.Sequence              as Seq
import qualified Data.Text                  as T
import           Data.Text.Encoding         (decodeUtf8)
import qualified Graphics.Vty.Input.Events  as Events
import           System.Environment         (getArgs)
import           System.Exit                (exitSuccess)
import           System.Posix.IO            (fdToHandle)
import           System.Posix.Types         (Fd (Fd))

import qualified Graphics.Vty as V

import qualified Brick.Main as M
import qualified Brick.Types as T
import Brick.Widgets.Core
  ( (<+>)
  , (<=>)
  , hLimit
  , vLimit
  , str
  )
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit as E
import qualified Brick.AttrMap as A
import Brick.Util (on)

import           JsonLogViewer.Filtration   (LogFilter (..),
                                             matchFilter)
import           JsonLogViewer.Settings     (getSettingsFilePath,
                                             loadSettingsFile,
                                             writeSettingsFile)
import           JsonLogViewer.StreamLines  (LinesCallback (..), streamLines)

-- purely informative type synonyms
type FilterName = T.Text
type Filters = [LogFilter]
type Message = (Bool, Aeson.Value)
type PinnedListEntry = (Int, Aeson.Value) -- ^ The Int is an index into the
                                          -- Messages sequence.
type FiltersIndex = Int -- ^ Index into the FiltersRef sequence

{-
So I want to switch this to using Brick.

Brick wants there to be one big application state. I love that! What should it
look like? How about something like this:
-}




theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (E.editAttr, V.white `on` V.blue)
    ]

data JsonLogViewerState = JsonLogViewerState
    { messages :: Seq.Seq Message
    , currentlyViewing :: Maybe Message
    , pinnedMessages :: Seq.Seq PinnedListEntry
    , filters :: [(Bool, LogFilter)] -- ^ Bool is whether the filter is active
    , currentlyFollowing :: Bool
    , columns :: [T.Text]
    }

theApp :: M.App JsonLogViewerState V.Event
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = M.neverShowCursor
          , M.appHandleEvent = M.resizeOrQuit
          , M.appStartEvent = return
          , M.appAttrMap = def
          , M.appLiftVtyEvent = id
          }


initialState :: JsonLogViewerState
initialState =
    JsonLogViewerState { messages = mempty
                       , currentlyViewing = Nothing
                       , pinnedMessages = mempty
                       , filters = mempty
                       , currentlyFollowing = False
                       , columns = mempty}


drawUI :: JsonLogViewerState -> [T.Widget]
drawUI st = [str "Hello, world!"]

-- |Return True if the message should be shown in the UI (if it's pinned or
-- matches filters)
shouldShowMessage :: [LogFilter] -> Message -> Bool
shouldShowMessage filters (pinned, json) = pinned || all (`matchFilter` json) activeFilters
  where activeFilters = filter (filterIsActive) filters

-- super general utilities

replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex index item ls = a ++ (item:b) where (a, _:b) = splitAt index ls

jsonToText :: Aeson.Value -> T.Text
jsonToText = decodeUtf8 . BSL.toStrict . Aeson.encode

removeSlice :: Int -> Int -> [a] -> [a]
removeSlice n m xs = take n xs ++ drop m xs

removeIndex :: Int -> [a] -> [a]
removeIndex n = removeSlice n (n+1)


-- |Given a list of columns and a json value, format it.
columnify :: [T.Text] -> Aeson.Value -> T.Text
columnify columns message =
  case message of
       (Aeson.Object hashmap) -> T.concat $ fmap (getColumn hashmap) columns
       _ -> "not an object"
  where renderWithKey key str = key `T.append` "=" `T.append` str `T.append` " "
        getColumn hashmap column =
          case HM.lookup column hashmap of
           (Just (Aeson.String str)) ->
             if "\n" `T.isInfixOf` str -- the list widgets won't show anything
                                       -- after a newline, so don't include them
             then renderWithKey column (T.pack $ show str)
             else renderWithKey column str
           (Just (Aeson.Number num)) -> renderWithKey column $ T.pack $ show num
           (Just x) -> renderWithKey column (T.pack $ show x)
           _ -> ""


-- |Given a list of columns and a json value, format it. If columns is null,
-- the plain json is returned.
formatMessage :: [T.Text] -> Aeson.Value -> T.Text
formatMessage columns message = if null columns
                                then jsonToText message
                                else columnify columns message


headerText :: T.Text
headerText = "ESC=Exit, TAB=Switch section, D=Delete filter, \
             \SPC=Toggle Filter, RET=Open, \
             \P=Pin message, F=Create Filter, C=Change Columns, E=Follow end, \
             \Ctrl+s=Save settings"


formatFilterForList :: LogFilter -> T.Text
formatFilterForList filt = T.append bullet (filterName filt)
  where bullet = (if filterIsActive filt then "* " else "- ")


-- |Convert lines from our streaming process to Messages
byteStringsToMessages :: [BS.ByteString] -> Seq.Seq Message
byteStringsToMessages newLines =
  let decoder = Aeson.decode :: BSL.ByteString -> Maybe Aeson.Value
      newJsons = mapMaybe (decoder . BSL.fromStrict) newLines
  in Seq.fromList $ map (False,) newJsons


usage :: String
usage = " \n\
\USAGE: json-log-viewer 3< <(tail -F LOGFILE) -- for streaming\n\
\       json-log-viewer 3< LOGFILE -- to load a file without streaming\n\
\\n\
\Input must be JSON documents, one per line.\n\
\\n\
\This syntax works with bash and zsh.\n\
\\n\
\fish supports file redirection but I haven't figured out the syntax for\n\
\subprocess redirection."


main :: IO ()
main = do
  args <- getArgs
  case args of
        [] -> startApp
        ["-h"] -> error usage
        ["--help"] -> error usage
        _ -> error usage


startApp :: IO ()
startApp = do
  -- load settings
  fp <- getSettingsFilePath
  (filters, columns) <- loadSettingsFile fp
  st <- M.defaultMain theApp initialState
  putStrLn "Done."
