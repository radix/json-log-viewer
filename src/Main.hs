{-# LANGUAGE NamedWildCards            #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE PatternGuards             #-}
{-# LANGUAGE TupleSections             #-}

module Main where

import           Control.Concurrent         (forkIO, threadDelay)
import           Control.Concurrent.MVar    (MVar (..), putMVar, takeMVar)
import           Control.Exception          (tryJust)
import           Control.Monad              (forM_, forever, guard, when)
import qualified Data.Aeson                 as Aeson
import           Data.Aeson.Encode.Pretty   (encodePretty)
import qualified Data.ByteString.Lazy       as BS
import qualified Data.ByteString.Lazy.Char8 as C8
import           Data.Foldable              (toList)
import qualified Data.HashMap.Strict        as HM
import           Data.IORef                 (IORef, modifyIORef, newIORef,
                                             readIORef, writeIORef)
import           Data.Maybe                 (catMaybes, maybe)
import qualified Data.Sequence              as Seq
import qualified Data.Text                  as T
import qualified Graphics.Vty.Input.Events  as Events
import qualified Graphics.Vty.Widgets.All   as UI
import           System.Environment         (getArgs)
import           System.Exit                (exitSuccess)
import           System.IO                  (IOMode (ReadMode),
                                             SeekMode (AbsoluteSeek), hSeek,
                                             openFile)
import           System.IO.Error            (isDoesNotExistError)
import           System.Posix.Files         (fileSize, getFileStatus)

import           Data.Aeson.Path
import           Data.Aeson.Path.Parser
import           TailF                      (streamLines)

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

makeMainWindow messagesRef filtersRef = do
  mainHeader <- UI.plainText "json-log-viewer by radix. ESC=Exit, TAB=Switch section, DEL=Delete filter, RET=Inspect, P=Pin message, C=Create Filter"
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
        let filterItems = zip (fmap snd namesAndFilters) filterWidgets
        UI.clearList filterList
        UI.addMultipleToList filterList filterItems
        -- update messages list
        UI.clearList messageList
        addMessagesToUI namesAndFilters messageList messages
      addMessages newMessages = do
        namesAndFilters <- readIORef filtersRef
        addMessagesToUI namesAndFilters messageList newMessages

  refreshMessages
  return (ui, messageList, filterList, refreshMessages, addMessages)

addMessagesToUI :: Filters -> _Widget -> Messages -> IO ()
addMessagesToUI filters messageList newMessages = do
  let filteredMessages = toList $ filterMessages newMessages (fmap snd filters)
  messageWidgets <- mapM (UI.plainText . jsonToText) filteredMessages
  let messageItems = zip filteredMessages messageWidgets
  UI.addMultipleToList messageList messageItems


makeMessageDetailWindow = do
  mdHeader <- UI.plainText "Message Detail. ESC=return"
  mdHeader <- UI.bordered mdHeader
  mdBody <- UI.plainText "Insert message here."
  messageDetail <- UI.vBox mdHeader mdBody
  return (messageDetail, mdBody)

makeFilterCreationWindow filtersRef refreshMessages switchToMain = do
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

  (filterDialog, filterFg) <- UI.newDialog dialogBody "Create New Filter"
  let filterCreationWindow = UI.dialogWidget filterDialog

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
    let path = getPath $ T.unpack text
    case path of
     (Left error) -> UI.setText parseStatusText $ T.pack $ show error
     (Right parse) -> UI.setText parseStatusText "Valid! :-)"

  filterFg `UI.onKeyPressed` \_ key _ ->
    case key of
     Events.KEsc -> UI.cancelDialog filterDialog >> return True
     _ -> return False

  filterDialog `UI.onDialogAccept` \_ -> do
    pathText <- UI.getEditText jsonPathEdit
    operand <- UI.getEditText operandEdit
    currentRadio <- UI.getCurrentRadio operatorRadioGroup
    nameText <- UI.getEditText nameEdit
    let predicate = case currentRadio of
          Just rButton
            | rButton == equalsCheck -> Equals $ Aeson.String operand
            | rButton == substringCheck -> HasSubstring operand
            | rButton == hasKeyCheck -> HasKey operand
          _ -> error "shouldn't happen because there's a default"
    let mpath = getPath $ T.unpack pathText
    case mpath of
     Right path -> do
       let filt = Filter path predicate
       modifyIORef filtersRef ((nameText, filt):)
       refreshMessages
       switchToMain
     Left e -> return ()

  filterDialog `UI.onDialogCancel` const switchToMain

  return (filterCreationWindow, nameEdit, filterFg, filterDialog)


-- TODO: Is it okay to use C8.unpack?
jsonToText :: Aeson.Value -> T.Text
jsonToText = T.pack . C8.unpack . Aeson.encode

removeSlice :: Int -> Int -> [a] -> [a]
removeSlice n m xs = take n xs ++ drop m xs

removeIndex :: Int -> [a] -> [a]
removeIndex n = removeSlice n (n+1)


type Filters = [(FilterName, Filter)]
type Messages = Seq.Seq (IsPinned, Aeson.Value)
type MessagesRef = IORef Messages
type FiltersRef = IORef Filters


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
      let newMessages = Seq.fromList $ map (False,) $ catMaybes $ map Aeson.decode newLines :: Seq.Seq (IsPinned, Aeson.Value)
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

  -- UI stuff

  -- main window
  (ui, messageList, filterList, refreshMessages, addMessages) <- makeMainWindow messagesRef filtersRef

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
  (filterCreationWindow,
   filterNameEdit,
   filterFg,
   filterDialog) <- makeFilterCreationWindow filtersRef refreshMessages switchToMain
  switchToFilterCreation <- UI.addToCollection c filterCreationWindow filterFg

  mainFg `UI.onKeyPressed` \_ key _ ->
    case key of
     Events.KEsc -> exitSuccess
     (Events.KChar 'f') -> do
       UI.focus filterNameEdit
       switchToFilterCreation
       return True
     _ -> return False

  messageList `UI.onItemActivated` \(UI.ActivateItemEvent _ message _) -> do
    let pretty = T.pack $ C8.unpack $ encodePretty message
    UI.setText mdBody pretty
    switchToMessageDetail

  let editFilter = return . Just . ("foo",) -- todo: define editFilter to pop up a thing and do a thing.

  filterList `UI.onItemActivated` \(UI.ActivateItemEvent index filt _) -> do
    maybeNewFilter <- editFilter filt
    case maybeNewFilter of
     Just nameAndFilter -> do
       modifyIORef filtersRef (
         \filters -> (take index filters) ++ [nameAndFilter] ++ (drop (index+1) filters))
       refreshMessages
     Nothing -> return ()

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
