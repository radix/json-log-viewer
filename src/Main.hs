{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE NamedWildCards            #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE PatternGuards             #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TupleSections             #-}

module Main where


import           Control.Concurrent        (forkIO)
import           Control.Exception         (tryJust)
import           Control.Monad             (forM_, forever, guard, liftM, mzero,
                                            when)
import qualified Data.Aeson                as Aeson
import           Data.Aeson.Encode.Pretty  (encodePretty)
import qualified Data.Aeson.Types          as Aeson.Types
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Lazy      as BSL
import           Data.Char                 (isSpace)
import           Data.Foldable             (toList)
import qualified Data.HashMap.Strict       as HM
import           Data.IORef                (IORef, modifyIORef, newIORef,
                                            readIORef, writeIORef)
import           Data.Maybe                (fromMaybe, mapMaybe)
import qualified Data.Sequence             as Seq
import qualified Data.Text                 as T
import           Data.Text.Encoding        (decodeUtf8)
import qualified Data.Vector               as V
import qualified Graphics.Vty.Attributes   as Attrs
import qualified Graphics.Vty.Input.Events as Events
import qualified Graphics.Vty.Widgets.All  as UI
import           System.Directory          (createDirectoryIfMissing,
                                            getHomeDirectory)
import           System.Environment        (getArgs)
import           System.Exit               (exitSuccess)
import           System.FilePath           (splitFileName, (</>))
import           System.IO                 (BufferMode (LineBuffering), Handle,
                                            hIsSeekable, hSetBuffering,
                                            hWaitForInput)
import           System.IO.Error           (isDoesNotExistError)
import           System.Posix.IO           (fdToHandle)
import           System.Posix.Types        (Fd (Fd))

import           Data.Aeson.Path           (JSONPath (..), followPath)
import qualified Data.Aeson.Path.Parser    as Parser

-- FILTRATION

data JSONPredicate
  = Equals Aeson.Value
  | MatchesRegex T.Text
  | HasSubstring T.Text
  | HasKey T.Text
  deriving Show

data LogFilter = LogFilter
  { jsonPath       :: JSONPath
  , jsonPredicate  :: JSONPredicate
  , filterName     :: T.Text
  , filterIsActive :: IsActive} deriving Show

lArray :: [Aeson.Value] -> Aeson.Value
lArray = Aeson.Array . V.fromList

instance Aeson.ToJSON JSONPredicate where
  toJSON (Equals jsonVal) = lArray [Aeson.String "Equals", jsonVal]
  toJSON (MatchesRegex text) = lArray [Aeson.String "MatchesRegex", Aeson.String text]
  toJSON (HasSubstring text) = lArray [Aeson.String "HasSubstring", Aeson.String text]
  toJSON (HasKey text) = lArray [Aeson.String "HasKey", Aeson.String text]

instance Aeson.FromJSON JSONPredicate where
  parseJSON (Aeson.Array v) = case toList v of
    [Aeson.String "Equals", val] -> pure $ Equals val
    [Aeson.String "MatchesRegex", Aeson.String text] -> pure $ MatchesRegex text
    [Aeson.String "HasSubstring", Aeson.String text] -> pure $ HasSubstring text
    [Aeson.String "HasKey", Aeson.String text] -> pure $ HasKey text
  parseJSON _ = mzero

instance Aeson.ToJSON LogFilter where
  toJSON (LogFilter {..}) = Aeson.object [
    "name" Aeson..= filterName
    , "is_active" Aeson..= filterIsActive
    , "path" Aeson..= Parser.toString jsonPath
    , "predicate" Aeson..= Aeson.toJSON jsonPredicate]

instance Aeson.FromJSON LogFilter where
  parseJSON (Aeson.Object o) = do
    -- I'd *like* to do this applicatively, but I'm not actually sure it's
    -- possible at all, given the Either handling below.
    name <- o Aeson..: "name"
    predicate <- o Aeson..: "predicate"
    pathText <- o Aeson..: "path"
    isActive <- o Aeson..: "is_active"
    let parsed = Parser.getPath pathText
    case parsed of
     Right path -> return LogFilter {filterName=name, filterIsActive=isActive,
                                     jsonPredicate=predicate, jsonPath=path}
     Left e -> fail ("Error when parsing jsonPath: " ++ show e)
  parseJSON _ = mzero

matchPredicate :: JSONPredicate -> Aeson.Value -> Bool
matchPredicate (Equals expected)       got = expected == got
matchPredicate (HasSubstring expected) (Aeson.String got) = expected `T.isInfixOf` got
matchPredicate (MatchesRegex _) _ = error "Implement MatchesRegex"
matchPredicate (HasKey expected) (Aeson.Object hm) = HM.member expected hm
matchPredicate _ _ = False

matchFilter :: LogFilter -> Aeson.Value -> Bool
matchFilter (LogFilter {jsonPath, jsonPredicate}) aesonValue
  | Just gotValue <- followPath jsonPath aesonValue
    = jsonPredicate `matchPredicate` gotValue
matchFilter _ _ = False

filterMessages :: Seq.Seq (IsPinned, Aeson.Value) -> [LogFilter] -> Seq.Seq Aeson.Value
filterMessages messages filters = snd <$> Seq.filter filt messages
  where filt (pinned, json) = pinned || all (`matchFilter` json) activeFilters
        activeFilters = filter filterIsActive filters


-- purely informative type synonyms
type IsPinned = Bool
type IsActive = Bool
type FilterName = T.Text
type Filters = [LogFilter]
type Messages = Seq.Seq (IsPinned, Aeson.Value)
type MessagesRef = IORef Messages
type FiltersRef = IORef Filters
type ColumnsRef = IORef [T.Text]
type CurrentlyFollowing = Bool
type FiltersIndex = Int -- ^ Index into the FiltersRef sequence

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
formatMessage columns message= if null columns
                                then jsonToText message
                                else columnify columns message

-- UI code. It's groooooss.

-- |A bag of junk useful for filter creation/editing UIs
data FilterDialog = FilterDialog {
  nameEdit             :: UI.Widget UI.Edit
  , jsonPathEdit       :: UI.Widget UI.Edit
  , operandEdit        :: UI.Widget UI.Edit
  , operatorRadioGroup :: UI.RadioGroup
  , equalsCheck        :: UI.Widget (UI.CheckBox Bool)
  , substringCheck     :: UI.Widget (UI.CheckBox Bool)
  , hasKeyCheck        :: UI.Widget (UI.CheckBox Bool)
  , filterDialog       :: UI.Dialog
  , filterFg           :: UI.Widget UI.FocusGroup
  , setDefaults        :: IO ()
  }

makeField :: (Show a) => T.Text -> UI.Widget a -> IO (UI.Widget _W)
makeField labelText widget = do
  label <- UI.plainText labelText
  UI.hBox label widget

makeEditField :: T.Text -> IO (UI.Widget UI.Edit, UI.Widget _W)
makeEditField labelText = do
  edit <- UI.editWidget
  field <- makeField labelText edit
  return (edit, field)


headerText :: T.Text
headerText = "ESC=Exit, TAB=Switch section, D=Delete filter, RET=Open, \
             \P=Pin message, F=Create Filter, C=Change Columns, E=Follow end, \
             \Ctrl+s=Save settings"

makeMainWindow
  :: MessagesRef -> FiltersRef -> IORef Bool -> ColumnsRef
  -> IO (UI.Widget _W, -- ^ main widget
         UI.Widget (UI.List Aeson.Types.Value UI.FormattedText), -- ^ message list
         UI.Widget (UI.List LogFilter UI.FormattedText), -- ^ filter list
         IO (), -- ^ refreshMessages
         Seq.Seq (IsPinned, Aeson.Types.Value) -> IO (), -- ^ addmessage
         UI.Widget UI.Edit -- ^ column edit widget
        )
makeMainWindow messagesRef filtersRef followingRef columnsRef = do
  mainHeader <- UI.plainText headerText
  borderedMainHeader <- UI.bordered mainHeader

  messageHeader <- UI.plainText "Messages"
  messageList <- UI.newList 1
  borderedMessages <- UI.bordered messageList

  filterHeader <- UI.plainText "Filters"
  filterList <- UI.newList 1
  borderedFilters <- UI.bordered filterList

  columnsLabel <- UI.plainText "Columns:"
  columnsEdit <- UI.editWidget
  columnEditor <- UI.hBox columnsLabel columnsEdit

  messagesAndHeader <- UI.vBox messageHeader borderedMessages
  rightArea <- UI.vBox columnEditor messagesAndHeader
  leftArea <- UI.vBox filterHeader borderedFilters
  hb <- UI.hBox leftArea rightArea
  UI.setBoxChildSizePolicy hb $ UI.Percentage 15
  headerAndBody <- UI.vBox borderedMainHeader hb
  ui <- UI.centered headerAndBody

  let refreshMessages = do
        messages <- readIORef messagesRef
        filters <- readIORef filtersRef
        -- update filters list
        let renderFilter filt = if (filterIsActive filt)
                                then T.append "* " $ filterName filt
                                else T.append "- " $ filterName filt
        filterWidgets <- mapM (UI.plainText . renderFilter) filters
        let filterItems = zip filters filterWidgets
        UI.clearList filterList
        UI.addMultipleToList filterList filterItems
        -- update messages list
        UI.clearList messageList
        currentlyFollowing <- readIORef followingRef
        columns <- readIORef columnsRef
        UI.setEditText columnsEdit $ T.intercalate ", " columns
        addMessagesToUI filters columns messageList messages currentlyFollowing
      addMessages newMessages = do
        modifyIORef messagesRef (Seq.>< newMessages)
        filters <- readIORef filtersRef
        currentlyFollowing <- readIORef followingRef
        columns <- readIORef columnsRef
        addMessagesToUI filters columns messageList newMessages currentlyFollowing
      toggleSelectedFilterActive = do
        selection <- UI.getSelected filterList
        case selection of
         Just (index, (filt, _)) -> do
           let newFilt = filt {filterIsActive=not (filterIsActive filt)}
           modifyIORef filtersRef $ replaceAtIndex index newFilt
           refreshMessages

  messageList `UI.onKeyPressed` \_ key _ ->
    case key of
      Events.KHome -> UI.scrollToBeginning messageList >> return True
      Events.KEnd -> UI.scrollToEnd messageList >> return True
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

  refreshMessages
  return (ui, messageList, filterList, refreshMessages, addMessages, columnsEdit)

addMessagesToUI :: Filters -> [T.Text] -> _Widget -> Messages -> CurrentlyFollowing -> IO ()
addMessagesToUI filters columns messageList newMessages currentlyFollowing = do
  let filteredMessages = toList $ filterMessages newMessages filters
  messageWidgets <- mapM (UI.plainText . formatMessage columns) filteredMessages
  let messageItems = zip filteredMessages messageWidgets
  UI.addMultipleToList messageList messageItems
  when currentlyFollowing $ UI.scrollToEnd messageList

makeMessageDetailWindow :: IO (UI.Widget _W, UI.Widget UI.FormattedText)
makeMessageDetailWindow = do
  mdHeader' <- UI.plainText "Message Detail. ESC=return"
  mdHeader <- UI.bordered mdHeader'
  mdBody <- UI.plainText "Insert message here."
  messageDetail <- UI.vBox mdHeader mdBody
  return (messageDetail, mdBody)

-- |Create a general filter UI.
makeFilterDialog :: T.Text -> IO () -> IO FilterDialog
makeFilterDialog dialogName switchToMain = do
  (nameEdit, nameField) <- makeEditField "Filter Name:"
  (jsonPathEdit, jsonPathField) <- makeEditField "JSON Path:"
  parseStatusText <- UI.plainText "Please Enter a JSON Path."
  parseStatusField <- makeField "Parse status:" parseStatusText

  operatorLabel <- UI.plainText "Operator:"
  operatorRadioGroup <- UI.newRadioGroup
  equalsCheck <- UI.newCheckbox "Equals"
  substringCheck <- UI.newCheckbox "Has Substring"
  hasKeyCheck <- UI.newCheckbox "Has Key"
  UI.addToRadioGroup operatorRadioGroup equalsCheck
  UI.addToRadioGroup operatorRadioGroup substringCheck
  UI.addToRadioGroup operatorRadioGroup hasKeyCheck

  UI.setCheckboxChecked substringCheck
  operatorRadioChecks'' <- UI.hBox substringCheck hasKeyCheck
  operatorRadioChecks' <- UI.hBox equalsCheck operatorRadioChecks''
  operatorRadioChecks <- UI.hBox operatorLabel operatorRadioChecks'

  (operandEdit, operandField) <- makeEditField "Operand:"

  dialogBody <- UI.newTable [UI.column UI.ColAuto] UI.BorderNone
  let addRow = UI.addRow dialogBody
  addRow nameField
  addRow jsonPathField
  addRow parseStatusField
  addRow operatorRadioChecks
  addRow operandField

  (filterDialog, filterFg) <- UI.newDialog dialogBody dialogName

  let returnKeyMeansNext = [nameEdit, jsonPathEdit, operandEdit]
  forM_ returnKeyMeansNext (`UI.onActivate` (\_ -> UI.focusNext filterFg))

  let addFocus = UI.addToFocusGroup filterFg
  addFocus nameEdit
  addFocus jsonPathEdit
  addFocus equalsCheck
  addFocus substringCheck
  addFocus hasKeyCheck
  addFocus operandEdit

  jsonPathEdit `UI.onChange` \text -> do
    let path = Parser.getPath $ T.unpack text
    case path of
     (Left e) -> UI.setText parseStatusText $ T.pack $ show e
     (Right _) -> UI.setText parseStatusText "Valid! :-)"

  filterFg `UI.onKeyPressed` \_ key _ ->
    case key of
     Events.KEsc -> UI.cancelDialog filterDialog >> return True
     _ -> return False

  filterDialog `UI.onDialogCancel` const switchToMain

  let setDefaults = do
        UI.setEditText nameEdit ""
        UI.setEditText jsonPathEdit ""
        UI.setEditText operandEdit ""
        UI.setCheckboxChecked substringCheck
        UI.focus nameEdit

  return FilterDialog {
    nameEdit = nameEdit
    , jsonPathEdit = jsonPathEdit
    , equalsCheck = equalsCheck
    , substringCheck = substringCheck
    , hasKeyCheck = hasKeyCheck
    , filterDialog = filterDialog
    , filterFg = filterFg
    , setDefaults = setDefaults
    , operandEdit = operandEdit
    , operatorRadioGroup = operatorRadioGroup}



-- TODO radix: consider async + wait for getting the result of filter editing?

makeFilterEditWindow
  :: FiltersRef
  -> IO () -- ^ refreshMessages function
  -> IO () -- ^ switchToMain function
  -> IO (UI.Widget _W -- ^ the main widget
        , UI.Widget UI.FocusGroup -- ^ the dialog's focus group
        , FiltersIndex -> LogFilter -> IO ()) -- ^ the editFilter function
makeFilterEditWindow filtersRef refreshMessages switchToMain = do
  dialogRec <- makeFilterDialog "Edit Filter" switchToMain

  -- YUCK
  filterIndexRef <- newIORef (Nothing :: Maybe FiltersIndex)

  let editFilter index (LogFilter {jsonPath, jsonPredicate, filterName}) = do
        UI.focus (nameEdit dialogRec)
        UI.setEditText (nameEdit dialogRec) filterName
        UI.setEditText (jsonPathEdit dialogRec) $ Parser.toString jsonPath
        case jsonPredicate of
         (Equals jsonVal) -> do
           UI.setCheckboxChecked (equalsCheck dialogRec)
           case jsonVal of
            Aeson.String jsonText -> -- only supporting texts here for now
              UI.setEditText (operandEdit dialogRec) jsonText
         -- (MatchesRegex text) -> do return () -- TODO!
         (HasSubstring text) -> do
           UI.setCheckboxChecked (substringCheck dialogRec)
           UI.setEditText (operandEdit dialogRec) text
         (HasKey text) -> do
           UI.setCheckboxChecked (hasKeyCheck dialogRec)
           UI.setEditText (operandEdit dialogRec) text
        writeIORef filterIndexRef $ Just index

  filterDialog dialogRec `UI.onDialogAccept` \_ -> do
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

  return (UI.dialogWidget (filterDialog dialogRec), filterFg dialogRec, editFilter)


-- | Create a Filter based on the contents of a filter-editing dialog.
filterFromDialog :: FilterDialog -> IO (Maybe LogFilter)
filterFromDialog dialogRec = do
    pathText <- UI.getEditText (jsonPathEdit dialogRec)
    operand <- UI.getEditText (operandEdit dialogRec)
    currentRadio <- UI.getCurrentRadio (operatorRadioGroup dialogRec)
    nameText <- UI.getEditText (nameEdit dialogRec)
    let predicate = case currentRadio of
          Just rButton
            | rButton == equalsCheck dialogRec -> Equals $ Aeson.String operand
            | rButton == substringCheck dialogRec -> HasSubstring operand
            | rButton == hasKeyCheck dialogRec -> HasKey operand
          _ -> error "shouldn't happen because there's a default"
    let mpath = Parser.getPath $ T.unpack pathText
    case mpath of
     Right path -> do
       let filt = LogFilter {filterName=nameText,
                             jsonPath=path,
                             jsonPredicate=predicate,
                             filterIsActive=True}
       return $ Just filt
     Left _ -> return Nothing

makeFilterCreationWindow :: FiltersRef -> IO () -> IO () -> IO (UI.Widget _W, UI.Widget UI.FocusGroup, IO ())
makeFilterCreationWindow filtersRef refreshMessages switchToMain = do
  dialogRec <- makeFilterDialog "Create Filter" switchToMain

  filterDialog dialogRec `UI.onDialogAccept` \_ -> do
    maybeNameAndFilt <- filterFromDialog dialogRec
    case maybeNameAndFilt of
     Just filt -> do
       modifyIORef filtersRef (filt:)
       refreshMessages
       switchToMain
     Nothing -> return ()

  let createFilter = setDefaults dialogRec

  return (UI.dialogWidget (filterDialog dialogRec),
          filterFg dialogRec,
          createFilter)

getSettingsFilePath :: IO FilePath
getSettingsFilePath =
  getHomeDirectory `plus` (</> ".config"
                          </> "json-log-viewer"
                          </> "settings.json")
  where plus l r = liftM r l

writeSettingsFile :: FilePath -> Filters -> [T.Text] -> IO ()
writeSettingsFile fp filters columns = do
  let jsonBytes = encodePretty $ Aeson.object ["filters" Aeson..= filters,
                                               "columns" Aeson..= columns]
  createDirectoryIfMissing True $ fst $ splitFileName fp
  BSL.writeFile fp jsonBytes

parseSettings :: BSL.ByteString -> (Filters, [T.Text])
parseSettings bytes =
  let columnsAndFilters = do -- maybe monad
        result <- Aeson.decode bytes
        flip Aeson.Types.parseMaybe result $ \obj -> do -- parser monad
          columns <- obj Aeson..: "columns"
          filters <- obj Aeson..: "filters"
          return (filters, columns)
  in
   fromMaybe ([], []) columnsAndFilters

loadSettingsFile :: FilePath -> IO (Filters, [T.Text])
loadSettingsFile fp = do
  e <- tryJust (guard . isDoesNotExistError) (BSL.readFile fp)
  case e of
   Left _ -> return ([], [])
   Right bytes -> return $ parseSettings bytes

makeSaveSettingsDialog
  :: FiltersRef
  -> ColumnsRef
  -> IO () -- ^ switchToMain
  -> IO (UI.Widget _W, UI.Widget UI.FocusGroup)
makeSaveSettingsDialog filtersRef columnsRef switchToMain = do
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

  return (dialogWidget, dialogFg)

-- |Handle lines that are coming in from our "tail" process.
linesReceived
  :: (Messages -> IO ()) -- ^ the addMessages function
  -> [BS.ByteString] -- ^ the new line
  -> IO ()
linesReceived addMessages newLines = do
  -- potential for race condition here I think!
  -- hmm, or not, since this will be run in the vty-ui event loop.
  let decoder = Aeson.decode :: BSL.ByteString -> Maybe Aeson.Value
      newJsons = mapMaybe (decoder . BSL.fromStrict) newLines
      newMessages = Seq.fromList $ map (False,) newJsons
  addMessages newMessages


streamLines :: Handle -> ([BS.ByteString] -> IO ()) -> IO ()
streamLines handle callback = do
  isNormalFile <- hIsSeekable handle
  if isNormalFile then do
    -- in this case we just slurp the file and call the callback and stop
    callback =<< BS.split 10 <$> BS.hGetContents handle
  else do
    -- it's a real stream, so let's stream it
    hSetBuffering handle LineBuffering
    forever $ do
      availableLines <- hGetLines handle
      if (not $ null availableLines) then do
        callback availableLines
      else do
        -- fall back to a blocking read now that we've reached the end of the
        -- stream
        line <- BS.hGetLine handle
        callback [line]

-- |Get all the lines available from a handle, *hopefully* without blocking
-- This will sadly still block if there is a partial line at the end of the
-- stream and no more data is being written. hGetBufNonBlocking would allow me
-- to work around this problem, but then I'd have to keep my own buffer!
hGetLines :: Handle -> IO [BS.ByteString]
hGetLines handle = do
  readable <- hWaitForInput handle 0
  if readable then (:) <$> BS.hGetLine handle <*> hGetLines handle
  else do return []


usage :: String
usage = " \n\
\USAGE: json-log-viewer 3< <(tail -F LOGFILE) -- for streaming\n\
\       json-log-viewer 3< LOGFILE -- to load a file without streaming\n\
\\n\
\Input must be JSON documents, one per line.\n\
\\n\
\This syntax works with bash and zsh. \n\
\\n\
\fish supports file redirection but I haven't figured out the syntax for \n\
\pipe redirection."

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
  followingRef <- newIORef False :: IO (IORef CurrentlyFollowing)
  columnsRef <- newIORef [] :: IO ColumnsRef

  -- load settings
  fp <- getSettingsFilePath
  (filters, columns) <- loadSettingsFile fp
  writeIORef filtersRef filters
  writeIORef columnsRef columns

  -- UI stuff

  -- main window
  (ui,
   messageList,
   filterList,
   refreshMessages,
   addMessages,
   columnsEdit) <- makeMainWindow messagesRef filtersRef followingRef columnsRef

  handle <- fdToHandle $ Fd 3 -- I'm not sure if there's a reason to support
                              -- FDs other than 3?
  forkIO $ streamLines handle (UI.schedule . linesReceived addMessages)

  mainFg <- UI.newFocusGroup
  UI.addToFocusGroup mainFg messageList
  UI.addToFocusGroup mainFg filterList
  UI.addToFocusGroup mainFg columnsEdit
  c <- UI.newCollection
  switchToMain <- UI.addToCollection c ui mainFg

  -- message detail window
  (messageDetail, mdBody) <- makeMessageDetailWindow
  messageDetailFg <- UI.newFocusGroup
  switchToMessageDetail <- UI.addToCollection c messageDetail messageDetailFg

  -- filter creation window
  (filterCreationWidget,
   filterCreationFg,
   createFilter) <- makeFilterCreationWindow filtersRef refreshMessages switchToMain
  switchToFilterCreation <- UI.addToCollection c filterCreationWidget filterCreationFg

  -- filter edit window
  (filterEditWidget,
   filterEditFg,
   editFilter) <- makeFilterEditWindow filtersRef refreshMessages switchToMain
  switchToFilterEdit <- UI.addToCollection c filterEditWidget filterEditFg

  (saveSettingsWidget, saveSettingsFg) <- makeSaveSettingsDialog filtersRef columnsRef switchToMain
  switchToSaveSettingsDialog <- UI.addToCollection c saveSettingsWidget saveSettingsFg

  -- vty-ui, bafflingly, calls event handlers going from *outermost* first to
  -- the innermost widget last. So if we bind things on mainFg, they will
  -- override things like key input in the column text field. (!?)  So we apply
  -- the "mostly-global" key bindings to almost all widgets, but not the
  -- column-edit widget.
  let bindGlobalThings _ key mods = case (key, mods) of
        (Events.KChar 'q', []) -> exitSuccess
        (Events.KChar 'f', []) -> do
          createFilter
          switchToFilterCreation
          return True
        (Events.KChar 'e', []) -> do
          modifyIORef followingRef not
          UI.scrollToEnd messageList
          return True
        (Events.KChar 'j', []) -> do
          UI.scrollDown messageList
          return True
        (Events.KChar 'k', []) -> do
          UI.scrollUp messageList
          return True
        (Events.KChar 's', [Events.MCtrl]) -> do
          switchToSaveSettingsDialog
          return True
        _ -> return False

  mainFg `UI.onKeyPressed` \_ key _ ->
    case key of
     Events.KEsc -> exitSuccess
     _ -> return False

  messageList `UI.onKeyPressed` bindGlobalThings
  filterList `UI.onKeyPressed` bindGlobalThings

  messageList `UI.onItemActivated` \(UI.ActivateItemEvent _ message _) -> do
    let pretty = decodeUtf8 $ BSL.toStrict $ encodePretty message
    UI.setText mdBody pretty
    switchToMessageDetail

  filterList `UI.setSelectedUnfocusedAttr` Just (Attrs.defAttr `Attrs.withStyle` Attrs.reverseVideo)
  messageList `UI.setSelectedUnfocusedAttr` Just (Attrs.defAttr `Attrs.withStyle` Attrs.reverseVideo)

  filterList `UI.onItemActivated` \(UI.ActivateItemEvent index filt _) -> do
    editFilter index filt
    switchToFilterEdit

  filterList `UI.onKeyPressed` \_ key _ ->
    case key of
     (Events.KChar 'd') -> do
       selected <- UI.getSelected filterList
       case selected of
        Just (index, _) -> do
          UI.removeFromList filterList index
          modifyIORef filtersRef (removeIndex index)
          refreshMessages
          return True
        Nothing -> return False
     _ -> return False


  messageDetailFg `UI.onKeyPressed` \_ key _ ->
    case key of
     Events.KEsc -> switchToMain >> return True
     _ -> return False

  UI.runUi c UI.defaultContext
