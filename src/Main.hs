{-# LANGUAGE NamedWildCards        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections         #-}

module Main where

{-

There are a few big gross things about this app
1. The code is poorly factored (my fault)
2. There is no good "model/view" layer in vty-ui. Dealing with the lists of
   messages, pinned messages, and filters, is tedious and error-prone. If
   List widgets wrapped and observed ListModels instead of making me
   manually manage everything in the list, everything would be a lot
   better. I could certainly implement such a layer myself but so far I
   have just suffered through it.
3. All the UI setup code is pointlessly in the IO monad. It seems so much
   better to me (a Haskell newbie, so take that with a grain of salt) that
   only runUi, various "set" functions, and event handlers should return IO,
   and there should be some pure API for constructing trees of
   widgets.
4. (I'm not so sure on this one) Widget types depend on their contents. I
   like the idea of strongly typing as much as possible, but it's annoyed me
   a few times. The annoyance is drastically lessened with use of GHC's new
   "partial type signatures", so I can just try to ignore the contents of
   widgets.
5. There's no show/hide mechanism in vty-ui, and the "Group" widget will
   only allow replacing a widget with the *same type* of widget (see #4).


Doing List as a view/model thing will be kind of tricky. For my use case it
would require the ability to indicate list items to show/hide.

-}

import           Control.Concurrent         (forkIO)
import           Control.Monad              (unless, when)
import qualified Data.Aeson                 as Aeson
import           Data.Aeson.Encode.Pretty   (encodePretty)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as BSL
import           Data.Char                  (isSpace)
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
import           Graphics.Vty.Widgets.All   ((<++>), (<-->))
import qualified Graphics.Vty.Widgets.All   as UI
import           System.Environment         (getArgs)
import           System.Exit                (exitSuccess)
import           System.Posix.IO            (fdToHandle)
import           System.Posix.Types         (Fd (Fd))

import           JsonLogViewer.FilterDialog (blankFilterDialog,
                                             filterFromDialog, makeFilterDialog,
                                             setDefaultsFromFilter)
import           JsonLogViewer.Filtration   (IsActive (..), LogFilter (..),
                                             matchFilter)
import           JsonLogViewer.Settings     (getSettingsFilePath,
                                             loadSettingsFile,
                                             writeSettingsFile)
import           JsonLogViewer.StreamLines  (LinesCallback (..), streamLines)
import           JsonLogViewer.UIUtils      (makeCoolList, makeEditField)

-- purely informative type synonyms
newtype IsPinned = IsPinned { unIsPinned :: Bool } deriving Show
type FilterName = T.Text
type Filters = [LogFilter]
type Message = (IsPinned, Aeson.Value)
type Messages = Seq.Seq Message
type MessagesRef = IORef Messages
type PinnedListEntry = (Int, Aeson.Value) -- ^ The Int is an index into the
                                          -- Messages sequence.
type FiltersRef = IORef Filters
type ColumnsRef = IORef [T.Text]
type CurrentlyFollowing = Bool
type CurrentlyFollowingRef = IORef CurrentlyFollowing
type FiltersIndex = Int -- ^ Index into the FiltersRef sequence

-- Widgets
type MessageListWidget = UI.Widget (UI.List Int UI.FormattedText)
type FilterListWidget = UI.Widget (UI.List LogFilter UI.FormattedText)
type PinnedListWidget = UI.Widget (UI.List PinnedListEntry UI.FormattedText)


-- |Return True if the message should be shown in the UI (if it's pinned or
-- matches filters)
shouldShowMessage :: [LogFilter] -> (IsPinned, Aeson.Value) -> Bool
shouldShowMessage filters (pinned, json) = unIsPinned pinned || all (`matchFilter` json) activeFilters
  where activeFilters = filter (unIsActive . filterIsActive) filters

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
  where getColumn hashmap column =
          case HM.lookup column hashmap of
           (Just (Aeson.String str)) ->
             if "\n" `T.isInfixOf` str -- the list widgets won't show anything
                                       -- after a newline, so don't include them
             then renderWithKey column (T.pack $ show str)
             else renderWithKey column str
           (Just (Aeson.Number num)) -> renderWithKey column $ T.pack $ show num
           (Just x) -> renderWithKey column (T.pack $ show x)
           _ -> ""
        renderWithKey key str = key `T.append` "=" `T.append` str `T.append` " "

-- |Given a list of columns and a json value, format it. If columns is null,
-- the plain json is returned.
formatMessage :: [T.Text] -> Aeson.Value -> T.Text
formatMessage columns message = if null columns
                                then jsonToText message
                                else columnify columns message


{-

Factoring ideas: Optimize for understanding the application.

There are a number of "modes" that the application can be in:
- messages view
- message detail
- filter creation
- filter editing
- saving settings
The main screen needs to be able to switch to any of the other modes, and
the other modes need to be able to switch back to the main screen.

Another axis of important things is *actions that can be taken*:
- view details of a message
- create a filter
- edit a filter
- pin/unpin a message
- process received messages
- refilter all messages (this is a secondary effect of other things)

So we could have a central AppState value, with the following functions, ideally:
- viewSelectedMessageDetail appState -- Or should it be passed the message?
- switchToMain appState
- createFilter appState
- editSelectedFilter appState -- Or should it be passed the filter?
- saveSettings appState
- pinSelectedMessage appState -- Or should it be passed the message?
- unPinSelectedMessage appState -- Or should it be passed the message?
- refreshMessages appState
- addMessage appState message


This is a pretty object-oriented design, where scopes are probably
unnecessarily large. But then, scopes are already unnecessarily large in the
current codebase, while having a completely unclear structure. So this would
certainly be a win over the current state of affairs.

-}

-- UI code. It's groooooss.

headerText :: T.Text
headerText = "ESC=Exit, TAB=Switch section, D=Delete filter, \
             \SPC=Toggle Filter, RET=Open, \
             \P=Pin message, F=Create Filter, C=Change Columns, E=Follow end, \
             \Ctrl+s=Save settings"

formatFilterForList :: LogFilter -> T.Text
formatFilterForList filt = T.append (if unIsActive $ filterIsActive filt
                                        then "* "
                                        else "- ")
                                    (filterName filt)


refreshMessagesUI
   :: MessagesRef
   -> FiltersRef
   -> CurrentlyFollowingRef
   -> ColumnsRef
   -> FilterListWidget
   -> MessageListWidget
   -> PinnedListWidget
   -> _W
   -> IO ()
refreshMessagesUI messagesRef filtersRef followingRef columnsRef filterList messageList pinnedList columnsEdit = do
    messages <- readIORef messagesRef
    filters <- readIORef filtersRef
    -- update filters list
    filterWidgets <- mapM (UI.plainText . formatFilterForList) filters
    let filterItems = zip filters filterWidgets
    UI.clearList filterList
    UI.addMultipleToList filterList filterItems
    -- update messages list
    UI.clearList messageList
    currentlyFollowing <- readIORef followingRef
    columns <- readIORef columnsRef
    UI.setEditText columnsEdit $ T.intercalate ", " columns
    addMessagesToUI 0 filters columns messageList messages currentlyFollowing
    -- update pinned messages (in case columns changed)
    pinnedSize <- UI.getListSize pinnedList
    let rerenderPinned index = do
         mPinnedWidget <- UI.getListItem pinnedList index
         case mPinnedWidget of
          Just ((_, json), widget) -> UI.setText widget $ formatMessage columns json
          Nothing -> error "could't find pinned item when going through what should be all of its valid indices"
    mapM_ rerenderPinned [0..pinnedSize - 1]


makeMainWindow
  :: UI.Collection -> MessagesRef -> FiltersRef -> IORef Bool -> ColumnsRef
  -> IO (MessageListWidget,
         FilterListWidget,
         PinnedListWidget,
         IO (), -- ^ refreshMessages
         Seq.Seq (IsPinned, Aeson.Value) -> IO (), -- ^ addmessage
         IO () -- ^ switchToMain
        )
makeMainWindow collection messagesRef filtersRef followingRef columnsRef = do
  mainHeader <- UI.plainText headerText

  (messageList, messagesWidget) <- makeCoolList 1 "Messages"
  (pinnedList, pinBorder) <- makeCoolList 1 "Pinned Messages"
  (filterList, borderedFilters) <- makeCoolList 1 "Filters"
  (columnsEdit, columnsField) <- makeEditField "Columns:"

  top <- return borderedFilters <++> (return columnsField
                                       <-->
                                       return pinBorder)
  UI.setBoxChildSizePolicy top $ UI.Percentage 20
  body <- return top
          <-->
          return messagesWidget
  UI.setBoxChildSizePolicy body $ UI.Percentage 30
  headerAndBody <- return mainHeader
                   <-->
                   return body
  ui <- UI.centered headerAndBody

  let refreshMessages = do refreshMessagesUI messagesRef filtersRef followingRef columnsRef filterList messageList pinnedList columnsEdit
      addMessages newMessages = do
        messages <- readIORef messagesRef
        let oldLength = Seq.length messages
        writeIORef messagesRef (messages >< newMessages)
        filters <- readIORef filtersRef
        currentlyFollowing <- readIORef followingRef
        columns <- readIORef columnsRef
        addMessagesToUI oldLength filters columns messageList newMessages currentlyFollowing
      toggleSelectedFilterActive = do
        selection <- UI.getSelected filterList
        case selection of
         Just (index, (filt, _)) -> do
           let newFilt = filt {filterIsActive=IsActive $ not $ unIsActive $ filterIsActive filt}
           modifyIORef filtersRef $ replaceAtIndex index newFilt
           refreshMessages
         Nothing -> return ()
      pin index msg = do
        columns <- readIORef columnsRef
        modifyIORef messagesRef $ Seq.update index (IsPinned True, msg)
        listItemWidget <- UI.plainText $ formatMessage columns msg
        -- figure out the index at which we must insert the widget
        -- (to keep it sorted by messagelist index)
        mPinnedIndex <- UI.listFindFirstBy ((> index) . fst) pinnedList
        pinIndex <- case mPinnedIndex of
          Just pinnedIndex -> return pinnedIndex
          Nothing -> UI.getListSize pinnedList
        UI.insertIntoList pinnedList (index, msg) listItemWidget pinIndex
        UI.setSelected pinnedList pinIndex
      unPin index msg = do
        modifyIORef messagesRef $ Seq.update index (IsPinned False, msg)
        mPinnedIndex <- UI.listFindFirstBy ((== index) . fst) pinnedList
        case mPinnedIndex of
         Just pinnedIndex -> do
           _ <- UI.removeFromList pinnedList pinnedIndex
           return ()
         Nothing -> error "Couldn't find pinned list item but pinned was true, this shouldn't happen."

  messageList `UI.onKeyPressed` \_ key _ ->
    case key of
      Events.KHome -> UI.scrollToBeginning messageList >> return True
      Events.KEnd -> UI.scrollToEnd messageList >> return True
      (Events.KChar 'p') -> do
        selection <- UI.getSelected messageList
        case selection of
         Just (_, (index, _)) -> do
           -- Note that this index is the ListItem value, not the index into
           -- the UI.List.
           messages <- readIORef messagesRef
           let message = messages `Seq.index` index
               isPinned = unIsPinned $ fst message
               msgJson = snd message
           if not isPinned
             then pin index msgJson
             else unPin index msgJson
           return True
         Nothing -> return True
      _ -> return False

  pinnedList `UI.onKeyPressed` \_ key _ ->
    case key of
     (Events.KChar 'p') -> do
       selection <- UI.getSelected pinnedList
       case selection of
        -- note that the bound index here is the messageList index, not the
        -- pinnedList index.
        Just (_, ((index, msg), _)) -> unPin index msg
        Nothing -> return ()
       return True
     _ -> return False

  filterList `UI.onKeyPressed` \_ key _ ->
    case key of
      Events.KHome -> UI.scrollToBeginning filterList >> return True
      Events.KEnd -> UI.scrollToEnd filterList >> return True
      Events.KChar ' ' -> toggleSelectedFilterActive >> return True
      _ -> return False

  columnsEdit `UI.onActivate` \widg -> do
    columnsText <- UI.getEditText widg
    let columns = filter (not . T.null) $ T.split (\char -> char == ',' || isSpace char) columnsText
    writeIORef columnsRef columns
    refreshMessages
    UI.focus messageList

  mainFg <- UI.newFocusGroup
  _ <- UI.addToFocusGroup mainFg messageList
  _ <- UI.addToFocusGroup mainFg filterList
  _ <- UI.addToFocusGroup mainFg columnsEdit
  _ <- UI.addToFocusGroup mainFg pinnedList
  switchToMain <- UI.addToCollection collection ui mainFg

  mainFg `UI.onKeyPressed` \_ key _ ->
    case key of
     Events.KEsc -> exitSuccess
     _ -> return False

  refreshMessages
  return (messageList, filterList, pinnedList, refreshMessages, addMessages, switchToMain)


addMessagesToUI :: Int -> Filters -> [T.Text] -> MessageListWidget -> Messages -> CurrentlyFollowing -> IO ()
addMessagesToUI oldLength filters columns messageList newMessages currentlyFollowing = do
  let theRange = [oldLength..]
      -- Decorate the new messages with their *index into the messages model*,
      -- so that after we filter them we can remember their index as the
      -- model-value of each list item.
      newMessages' = toList newMessages
      decorated = zip theRange newMessages'
      filteredMessagesWithIndices = filter (shouldShowMessage filters . snd) decorated
  messageWidgets <- mapM (UI.plainText . formatMessage columns . snd . snd) filteredMessagesWithIndices
  let messageItems = zip (map fst filteredMessagesWithIndices) messageWidgets
  UI.addMultipleToList messageList messageItems
  when currentlyFollowing $ UI.scrollToEnd messageList


makeMessageDetailWindow
  :: UI.Collection
  -> IO () -- ^ switchToMain action
  -> IO (Aeson.Value -> IO ()) -- ^ A function for viewing message details
makeMessageDetailWindow collection switchToMain = do
  mdHeader' <- UI.plainText "Message Detail. ESC=return"
  mdHeader <- UI.bordered mdHeader'
  mdBody <- UI.plainText "Insert message here."
  messageDetail <- UI.vBox mdHeader mdBody
  messageDetailFg <- UI.newFocusGroup
  switchToMessageDetail <- UI.addToCollection collection messageDetail messageDetailFg
  messageDetailFg `UI.onKeyPressed` \_ key _ ->
    case key of
     Events.KEsc -> switchToMain >> return True
     _ -> return False
  let viewDetails value = do
        let pretty = decodeUtf8 $ BSL.toStrict $ encodePretty value
        UI.setText mdBody pretty
        switchToMessageDetail
  return viewDetails


makeFilterEditWindow
  :: UI.Collection
  -> FiltersRef
  -> IO () -- ^ refreshMessages function
  -> IO () -- ^ switchToMain function
  -> IO (Int -> LogFilter -> IO ()) -- ^ the editFilter function
makeFilterEditWindow collection filtersRef refreshMessages switchToMain = do
  (dialogRec, filterDialog, filterFg) <- makeFilterDialog "Edit Filter" switchToMain

  -- YUCK
  filterIndexRef <- newIORef (Nothing :: Maybe FiltersIndex)

  filterDialog `UI.onDialogAccept` \_ -> do
    maybeIndex <- readIORef filterIndexRef
    case maybeIndex of
     Nothing -> error "Error: trying to save an edited filter without knowing its index"
     Just index -> do
       maybeFilt <- filterFromDialog dialogRec
       case maybeFilt of
        Nothing -> return ()
        Just filt -> do
          modifyIORef filtersRef $ replaceAtIndex index filt
          refreshMessages
          writeIORef filterIndexRef Nothing
          switchToMain

  let filterEditWidget = UI.dialogWidget filterDialog
  switchToFilterEdit <- UI.addToCollection collection filterEditWidget filterFg
  let editFilter index logFilter = do
        setDefaultsFromFilter dialogRec logFilter
        writeIORef filterIndexRef $ Just index
        switchToFilterEdit

  return editFilter


makeFilterCreationWindow :: UI.Collection -> FiltersRef -> IO () -> IO () -> IO (IO ())
makeFilterCreationWindow collection filtersRef refreshMessages switchToMain = do
  (dialogRec, filterDialog, filterFg) <- makeFilterDialog "Create Filter" switchToMain

  filterDialog `UI.onDialogAccept` \_ -> do
    maybeNameAndFilt <- filterFromDialog dialogRec
    case maybeNameAndFilt of
     Just filt -> do
       modifyIORef filtersRef (filt:)
       refreshMessages
       switchToMain
     Nothing -> return ()

  let filterCreationWidget = UI.dialogWidget filterDialog
  switchToFilterCreation <- UI.addToCollection collection filterCreationWidget filterFg
  let createFilter = do blankFilterDialog dialogRec; switchToFilterCreation
  return createFilter


makeSaveSettingsDialog
  :: UI.Collection
  -> FiltersRef
  -> ColumnsRef
  -> IO () -- ^ switchToMain
  -> IO (IO ())
makeSaveSettingsDialog collection filtersRef columnsRef switchToMain = do
  settingsPath <- getSettingsFilePath
  dialogBody <- UI.plainText $ T.append "Filters and columns will be saved to " $ T.pack settingsPath
  (dialog, dialogFg) <- UI.newDialog dialogBody "Save settings?"
  let dialogWidget = UI.dialogWidget dialog

  dialogFg `UI.onKeyPressed` \_ key _ -> case key of
    Events.KEsc -> switchToMain >> return True
    _ -> return False

  dialog `UI.onDialogAccept` \_ -> do
    filters <- readIORef filtersRef
    columns <- readIORef columnsRef
    writeSettingsFile settingsPath filters columns
    switchToMain
    return ()

  dialog `UI.onDialogCancel` const switchToMain
  switchToSaveSettingsDialog <- UI.addToCollection collection dialogWidget dialogFg

  return switchToSaveSettingsDialog


-- |Convert lines from our streaming process to Messages
byteStringsToMessages :: [BS.ByteString] -> Messages
byteStringsToMessages newLines =
  let decoder = Aeson.decode :: BSL.ByteString -> Maybe Aeson.Value
      newJsons = mapMaybe (decoder . BSL.fromStrict) newLines
  in Seq.fromList $ map (IsPinned False,) newJsons


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
  -- mutable variablessss
  messagesRef <- newIORef Seq.empty :: IO MessagesRef
  filtersRef <- newIORef [] :: IO FiltersRef
  followingRef <- newIORef False :: IO CurrentlyFollowingRef
  columnsRef <- newIORef [] :: IO ColumnsRef

  -- load settings
  fp <- getSettingsFilePath
  (filters, columns) <- loadSettingsFile fp
  writeIORef filtersRef filters
  writeIORef columnsRef columns

  -- UI stuff
  c <- UI.newCollection

  -- main window
  (messageList,
   filterList,
   pinnedList,
   refreshMessages,
   addMessages,
   switchToMain) <- makeMainWindow c messagesRef filtersRef followingRef columnsRef

  handle <- fdToHandle $ Fd 3
  _ <- forkIO $ streamLines handle $ LinesCallback (UI.schedule . addMessages . byteStringsToMessages)

  -- Set up the various dialogs / modal views of the app
  viewMessageDetails <- makeMessageDetailWindow c switchToMain
  createFilter <- makeFilterCreationWindow c filtersRef refreshMessages switchToMain
  editFilter <- makeFilterEditWindow c filtersRef refreshMessages switchToMain
  saveSettings <- makeSaveSettingsDialog c filtersRef columnsRef switchToMain

  -- vty-ui, bafflingly, calls event handlers going from *outermost* first to
  -- the innermost widget last. So if we bind things on mainFg, they will
  -- override things like key input in the column text field. (!?)  So we apply
  -- the "mostly global" key bindings to almost all widgets, but not the
  -- column-edit widget.
  let bindGlobalThings _ key mods = case (key, mods) of
        (Events.KChar 'q', []) -> exitSuccess
        (Events.KChar 'f', []) -> do
          createFilter
          return True
        (Events.KChar 'e', []) -> do
          isFollowing <- readIORef followingRef
          unless isFollowing $ UI.scrollToEnd messageList
          modifyIORef followingRef not
          return True
        (Events.KChar 's', [Events.MCtrl]) -> do
          saveSettings
          return True
        _ -> return False

  messageList `UI.onKeyPressed` bindGlobalThings
  filterList `UI.onKeyPressed` bindGlobalThings
  pinnedList `UI.onKeyPressed` bindGlobalThings

  pinnedList `UI.onItemActivated` \(UI.ActivateItemEvent _ (_, message) _) ->
    viewMessageDetails message

  messageList `UI.onItemActivated` \(UI.ActivateItemEvent _ index _) -> do
    -- Note that this `index` is the UI.List "value", not the index into the
    -- List view.
    messages <- readIORef messagesRef
    viewMessageDetails $ snd $ messages `Seq.index` index

  filterList `UI.onItemActivated` \(UI.ActivateItemEvent index filt _) -> do
    editFilter index filt

  filterList `UI.onKeyPressed` \_ key _ ->
    case key of
     (Events.KChar 'd') -> do
       selected <- UI.getSelected filterList
       case selected of
        Just (index, _) -> do
          _ <- UI.removeFromList filterList index
          modifyIORef filtersRef (removeIndex index)
          refreshMessages
          return True
        Nothing -> return False
     _ -> return False

  UI.runUi c UI.defaultContext
