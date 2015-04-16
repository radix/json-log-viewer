{-# LANGUAGE NamedWildCards            #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE PatternGuards             #-}
{-# LANGUAGE TupleSections             #-}

module Main where

import           Control.Concurrent        (forkIO, threadDelay)
import           Control.Concurrent.MVar   (MVar (..), putMVar, takeMVar)
import           Control.Exception         (tryJust)
import           Control.Monad             (forM_, forever, guard, when)
import Data.List (splitAt)
import qualified Data.Aeson                as Aeson
import           Data.Aeson.Encode.Pretty  (encodePretty)
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Lazy      as BSL
import           Data.Foldable             (toList)
import qualified Data.HashMap.Strict       as HM
import           Data.IORef                (IORef, modifyIORef, newIORef,
                                            readIORef, writeIORef)
import           Data.Maybe                (catMaybes, maybe)
import qualified Data.Sequence             as Seq
import qualified Data.Text                 as T
import           Data.Text.Encoding        (decodeUtf8)
import qualified Graphics.Vty.Attributes   as Attrs
import qualified Graphics.Vty.Input.Events as Events
import qualified Graphics.Vty.Widgets.All  as UI
import           System.Environment        (getArgs)
import           System.Exit               (exitSuccess)
import           System.IO                 (IOMode (ReadMode),
                                            SeekMode (AbsoluteSeek), hSeek,
                                            openFile)
import           System.IO.Error           (isDoesNotExistError)
import           System.Posix.Files        (fileSize, getFileStatus)

import           Data.Aeson.Path
import qualified Data.Aeson.Path.Parser    as Parser
import           TailF                     (streamLines)

-- FILTRATION

data JSONPredicate
  = Equals Aeson.Value
  | MatchesRegex T.Text
  | HasSubstring T.Text
  | HasKey T.Text
  deriving Show

data Filter
  = Filter JSONPath JSONPredicate
  | AllFilters [Filter]
  | AnyFilter [Filter]
  deriving Show

matchPredicate :: JSONPredicate -> Aeson.Value -> Bool
matchPredicate (Equals expected)       got = expected == got
matchPredicate (HasSubstring expected) (Aeson.String got) = expected `T.isInfixOf` got
matchPredicate (MatchesRegex expected) _ = error "Implement MatchesRegex"
matchPredicate (HasKey expected) (Aeson.Object hm) = HM.member expected hm
matchPredicate _ _ = False

matchFilter :: Filter -> Aeson.Value -> Bool
matchFilter (Filter jspath predicate) aesonValue
  | Just gotValue <- jsonPath jspath aesonValue
    = predicate `matchPredicate` gotValue
matchFilter (AllFilters filters) aesonValue = all (`matchFilter` aesonValue) filters
matchFilter (AnyFilter filters) aesonValue = any (`matchFilter` aesonValue) filters
matchFilter _ _ = False


filterMessages :: Seq.Seq (IsPinned, Aeson.Value) -> [Filter] -> Seq.Seq Aeson.Value
filterMessages messages filters = fmap snd $ Seq.filter filt messages
  where filt (pinned, json) = pinned || matchFilter (AllFilters filters) json


-- purely informative type synonyms
type IsPinned = Bool
type IsActive = Bool
type FilterName = T.Text


-- UI code. It's groooooss.

makeField labelText widget = do
  label <- UI.plainText labelText
  UI.hBox label widget

makeEditField labelText = do
  edit <- UI.editWidget
  field <- makeField labelText edit
  return (edit, field)

makeMainWindow messagesRef filtersRef followingRef = do
  mainHeader <- UI.plainText "json-log-viewer by radix. ESC=Exit, TAB=Switch section, D=Delete filter, RET=Open, P=Pin message, C=Create Filter, F=Follow end"
  borderedMainHeader <- UI.bordered mainHeader
  messageHeader <- UI.plainText "Messages"
  messageList <- UI.newList 1
  filterHeader <- UI.plainText "Filters"
  filterList <- UI.newList 1
  borderedFilters <- UI.bordered filterList
  borderedMessages <- UI.bordered messageList
  messagesAndHeader <- UI.vBox messageHeader borderedMessages
  filtersAndHeader <- UI.vBox filterHeader borderedFilters
  hb <- UI.hBox filtersAndHeader messagesAndHeader
  UI.setBoxChildSizePolicy hb $ UI.Percentage 15
  headerAndBody <- UI.vBox borderedMainHeader hb
  ui <- UI.centered headerAndBody

  messageList `UI.onKeyPressed` \_ key _ ->
    case key of
      Events.KHome -> UI.scrollToBeginning messageList >> return True
      Events.KEnd -> UI.scrollToEnd messageList >> return True
      _ -> return False

  filterList `UI.onKeyPressed` \_ key _ ->
    case key of
      Events.KHome -> UI.scrollToBeginning filterList >> return True
      Events.KEnd -> UI.scrollToEnd filterList >> return True
      _ -> return False

  let refreshMessages = do
        messages <- readIORef messagesRef
        namesAndFilters <- readIORef filtersRef
        -- update filters list
        filterWidgets <- mapM (UI.plainText . fst) namesAndFilters
        let filterItems = zip namesAndFilters filterWidgets
        UI.clearList filterList
        UI.addMultipleToList filterList filterItems
        -- update messages list
        UI.clearList messageList
        currentlyFollowing <- readIORef followingRef
        addMessagesToUI namesAndFilters messageList messages currentlyFollowing
      addMessages newMessages = do
        namesAndFilters <- readIORef filtersRef
        currentlyFollowing <- readIORef followingRef
        addMessagesToUI namesAndFilters messageList newMessages currentlyFollowing

  refreshMessages
  return (ui, messageList, filterList, refreshMessages, addMessages)

addMessagesToUI :: Filters -> _Widget -> Messages -> CurrentlyFollowing -> IO ()
addMessagesToUI filters messageList newMessages currentlyFollowing = do
  let filteredMessages = toList $ filterMessages newMessages (fmap snd filters)
  messageWidgets <- mapM (UI.plainText . jsonToText) filteredMessages
  let messageItems = zip filteredMessages messageWidgets
  UI.addMultipleToList messageList messageItems
  when currentlyFollowing $ UI.scrollToEnd messageList


makeMessageDetailWindow = do
  mdHeader <- UI.plainText "Message Detail. ESC=return"
  mdHeader <- UI.bordered mdHeader
  mdBody <- UI.plainText "Insert message here."
  messageDetail <- UI.vBox mdHeader mdBody
  return (messageDetail, mdBody)

-- |A bag of junk useful for filter creation/editing UIs
data FilterDialog = FilterDialog {
  nameEdit :: UI.Widget UI.Edit
  , jsonPathEdit :: UI.Widget UI.Edit
  , operandEdit :: UI.Widget UI.Edit
  , operatorRadioGroup :: UI.RadioGroup
  , equalsCheck :: UI.Widget (UI.CheckBox Bool)
  , substringCheck :: UI.Widget (UI.CheckBox Bool)
  , hasKeyCheck :: UI.Widget (UI.CheckBox Bool)
  , filterDialog :: UI.Dialog
  , filterFg :: UI.Widget UI.FocusGroup
  , setDefaults :: IO ()
  }


-- |Create a general filter UI.
makeFilterDialog :: String -> _ -> IO FilterDialog
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
  operatorRadioChecks <- UI.hBox substringCheck hasKeyCheck
  operatorRadioChecks <- UI.hBox equalsCheck operatorRadioChecks

  (operandEdit, operandField) <- makeEditField "Operand:"

  dialogBody <- UI.newTable [UI.column UI.ColAuto] UI.BorderNone
  let addRow = UI.addRow dialogBody
  addRow nameField
  addRow jsonPathField
  addRow parseStatusField
  addRow operatorRadioChecks
  addRow operandField

  (filterDialog, filterFg) <- UI.newDialog dialogBody "Filter"

  let returnKeyMeansNext = [nameEdit, jsonPathEdit, operandEdit]
  forM_ returnKeyMeansNext (`UI.onActivate` (\x -> UI.focusNext filterFg))

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
     (Left error) -> UI.setText parseStatusText $ T.pack $ show error
     (Right parse) -> UI.setText parseStatusText "Valid! :-)"

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
  -> IO (UI.Widget _ -- ^ the main widget
        , UI.Widget UI.FocusGroup -- ^ the dialog's focus group
        , (FiltersIndex -> T.Text -> Filter -> IO ())) -- ^ the editFilter function
makeFilterEditWindow filtersRef refreshMessages switchToMain = do
  dialogRec <- makeFilterDialog "Edit Filter" switchToMain

  -- YUCK
  filterIndexRef <- newIORef (Nothing :: Maybe FiltersIndex)

  let editFilter index name filt = do
        UI.focus (nameEdit dialogRec)
        case filt of
         Filter jspath jspred -> do
           UI.setEditText (nameEdit dialogRec) name
           UI.setEditText (jsonPathEdit dialogRec) $ Parser.toString jspath
           case jspred of
            (Equals jsonVal) -> do
              UI.setCheckboxChecked (equalsCheck dialogRec)
              case jsonVal of
               Aeson.String jsonText -> do -- only supporting texts here for now
                 UI.setEditText (operandEdit dialogRec) jsonText
            -- (MatchesRegex text) -> do return () -- TODO!
            (HasSubstring text) -> do
              UI.setCheckboxChecked (substringCheck dialogRec)
              UI.setEditText (operandEdit dialogRec) text
            (HasKey text) -> do
              UI.setCheckboxChecked (hasKeyCheck dialogRec)
              UI.setEditText (operandEdit dialogRec) text
           writeIORef filterIndexRef $ Just index
         AllFilters filts -> error "editing multiple filters not supported"
         AnyFilter filts -> error "editing multiple filters not supported"

  (filterDialog dialogRec) `UI.onDialogAccept` \_ -> do
    maybeIndex <- readIORef filterIndexRef
    case maybeIndex of
     Nothing -> error "Error: trying to save an edited filter without knowing its index"
     Just index -> do
       maybeNameAndFilt <- filterFromDialog dialogRec
       case maybeNameAndFilt of
        Nothing -> return ()
        Just pair -> do
          modifyIORef filtersRef $ replaceAtIndex index pair
          refreshMessages
          writeIORef filterIndexRef Nothing
          switchToMain

  return (UI.dialogWidget (filterDialog dialogRec), filterFg dialogRec, editFilter)

replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex index item ls = a ++ (item:b) where (a, (_:b)) = splitAt index ls

filterFromDialog :: FilterDialog -> IO (Maybe (T.Text, Filter))
filterFromDialog dialogRec = do
    pathText <- UI.getEditText (jsonPathEdit dialogRec)
    operand <- UI.getEditText (operandEdit dialogRec)
    currentRadio <- UI.getCurrentRadio (operatorRadioGroup dialogRec)
    nameText <- UI.getEditText (nameEdit dialogRec)
    let predicate = case currentRadio of
          Just rButton
            | rButton == (equalsCheck dialogRec) -> Equals $ Aeson.String operand
            | rButton == (substringCheck dialogRec) -> HasSubstring operand
            | rButton == (hasKeyCheck dialogRec) -> HasKey operand
          _ -> error "shouldn't happen because there's a default"
    let mpath = Parser.getPath $ T.unpack pathText
    case mpath of
     Right path -> do
       let filt = Filter path predicate
       return $ Just (nameText, filt)
     Left e -> return Nothing

makeFilterCreationWindow :: FiltersRef -> IO () -> IO () -> IO (UI.Widget _, UI.Widget UI.FocusGroup, IO ())
makeFilterCreationWindow filtersRef refreshMessages switchToMain = do
  dialogRec <- makeFilterDialog "Create Filter" switchToMain

  (filterDialog dialogRec) `UI.onDialogAccept` \_ -> do
    maybeNameAndFilt <- filterFromDialog dialogRec
    case maybeNameAndFilt of
     Just (name, filt) -> do
       modifyIORef filtersRef ((name, filt):)
       refreshMessages
       switchToMain
     Nothing -> return ()

  let createFilter = do
        (setDefaults dialogRec)

  return (UI.dialogWidget (filterDialog dialogRec),
          filterFg dialogRec,
          createFilter)


jsonToText :: Aeson.Value -> T.Text
jsonToText = decodeUtf8 . BSL.toStrict . Aeson.encode

removeSlice :: Int -> Int -> [a] -> [a]
removeSlice n m xs = take n xs ++ drop m xs

removeIndex :: Int -> [a] -> [a]
removeIndex n = removeSlice n (n+1)


type Filters = [(FilterName, Filter)]
type Messages = Seq.Seq (IsPinned, Aeson.Value)
type MessagesRef = IORef Messages
type FiltersRef = IORef Filters
type CurrentlyFollowing = Bool
type FiltersIndex = Int -- ^ Index into the FiltersRef sequence


messageReceived
  :: MessagesRef
  -> FiltersRef
  -> (Messages -> IO ()) -- ^ the addMessages function
  -> Either String [BS.ByteString] -- ^ the new line
  -> IO ()
messageReceived messagesRef filtersRef addMessages newLines =
  -- potential for race condition here I think!
  -- hmm, or not, since this will be run in the vty-ui event loop.
  case newLines of
    Right newLines -> do
      filters <- fmap snd <$> readIORef filtersRef
      let newMessages = Seq.fromList $ map (False,) $ catMaybes $ map (Aeson.decode . BSL.fromStrict) newLines :: Seq.Seq (IsPinned, Aeson.Value)
      modifyIORef messagesRef (\messages -> messages Seq.>< newMessages)
      addMessages newMessages
    Left message -> error message


usage = "json-log-viewer [filename]"

main = do
  args <- getArgs
  let filename = case args of
        [] -> error usage
        ["-h"] -> error usage
        ["--help"] -> error usage
        [filename] -> filename
        _ -> error usage

  -- Global Messages and Filters

  messagesRef <- newIORef Seq.empty :: IO MessagesRef
  filtersRef <- newIORef [] :: IO FiltersRef
  followingRef <- newIORef False :: IO (IORef CurrentlyFollowing)

  -- UI stuff

  -- main window
  (ui, messageList, filterList, refreshMessages, addMessages) <- makeMainWindow messagesRef filtersRef followingRef

  forkIO $ streamLines filename 0 1000000 (UI.schedule . messageReceived messagesRef filtersRef addMessages)

  mainFg <- UI.newFocusGroup
  UI.addToFocusGroup mainFg messageList
  UI.addToFocusGroup mainFg filterList
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

  (filterEditWidget,
   filterEditFg,
   editFilter) <- makeFilterEditWindow filtersRef refreshMessages switchToMain
  switchToFilterEdit <- UI.addToCollection c filterEditWidget filterEditFg

  mainFg `UI.onKeyPressed` \_ key _ ->
    case key of
     Events.KEsc -> exitSuccess
     (Events.KChar 'q') -> exitSuccess
     (Events.KChar 'c') -> do
       createFilter
       switchToFilterCreation
       return True
     (Events.KChar 'f') -> do
       modifyIORef followingRef not
       UI.scrollToEnd messageList
       return True
     (Events.KChar 'j') -> do
       UI.scrollDown messageList
       return True
     (Events.KChar 'k') -> do
       UI.scrollUp messageList
       return True
     _ -> return False

  messageList `UI.onItemActivated` \(UI.ActivateItemEvent _ message _) -> do
    let pretty = decodeUtf8 $ BSL.toStrict $ encodePretty message
    UI.setText mdBody pretty
    switchToMessageDetail

  filterList `UI.setSelectedUnfocusedAttr` Just (Attrs.defAttr `Attrs.withStyle` Attrs.reverseVideo)
  messageList `UI.setSelectedUnfocusedAttr` Just (Attrs.defAttr `Attrs.withStyle` Attrs.reverseVideo)

  filterList `UI.onItemActivated` \(UI.ActivateItemEvent index (name, filt) _) -> do
    editFilter index name filt
    switchToFilterEdit

  filterList `UI.onKeyPressed` \_ key _ ->
    case key of
     (Events.KChar 'd') -> do
       selected <- UI.getSelected filterList
       case selected of
        Just (index, (item, widg)) -> do
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
