{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PatternGuards             #-}
{-# LANGUAGE TupleSections             #-}

module Main where

import           Control.Monad              (forM_)
import qualified Data.Aeson                 as Aeson
import           Data.Aeson.Encode.Pretty   (encodePretty)
import qualified Data.ByteString.Lazy       as BS
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.HashMap.Strict        as HM
import           Data.IORef                 (modifyIORef, newIORef, readIORef,
                                             writeIORef)
import           Data.Maybe                 (catMaybes, maybe)
import qualified Data.Text                  as T
import qualified Graphics.Vty.Input.Events  as Events
import qualified Graphics.Vty.Widgets.All   as UI
import           System.Environment         (getArgs)
import           System.Exit                (exitSuccess)

import           Data.Aeson.Path
import           Data.Aeson.Path.Parser


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
    = if predicate `matchPredicate` gotValue then
        True
      else
        False
matchFilter (AllFilters filters) aesonValue = all (flip matchFilter aesonValue) filters
matchFilter (AnyFilter filters) aesonValue = any (flip matchFilter aesonValue) filters
matchFilter _ _ = False


{-

TODO:
- activating/deactivating filters
- RET on filter = edit filter
- filter names
- grouping filters (any and all)
- tail a file, continuously apply filters
  - https://gist.github.com/ijt/1055731
- reading input from a file descriptor
- filter persistence
- pinning messages prevents them from scrolling off :S
- tabbing between message and  filter lists doesn't have visual indicator
-}


filterMessages :: [(IsPinned, Aeson.Value)] -> [Filter] -> [Aeson.Value]
filterMessages messages filters = map snd $ filter filt messages
  where filt (pinned, json) = pinned || matchFilter (AllFilters filters) json



-- UI!

makeField labelText widget = do
  label <- UI.plainText labelText
  field <- UI.hBox label widget
  return field

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
  let refreshMessages = do
        messages <- readIORef messagesRef
        filters <- readIORef filtersRef
        -- update filters list
        let addToFiltersList filter = do
              txtWidg <- UI.plainText $ T.pack $ show filter
              UI.addToList filterList filter txtWidg
        UI.clearList filterList
        mapM_ addToFiltersList filters
        -- update messages list
        let filteredMessages = filterMessages messages filters
        UI.clearList messageList
        let addToList message = do
              let line = jsonToText message
              txtWidg <- UI.plainText line
              UI.addToList messageList message txtWidg
        mapM_ addToList filteredMessages

  refreshMessages
  return (ui, messageList, filterList, refreshMessages)


makeMessageDetailWindow = do
  mdHeader <- UI.plainText "Message Detail. ESC=return"
  mdHeader <- UI.bordered mdHeader
  mdBody <- UI.plainText "Insert message here."
  messageDetail <- UI.vBox mdHeader mdBody
  return (messageDetail, mdBody)

makeFilterCreationWindow filtersRef refreshMessages switchToMain = do
  {-
   TODO:
   - sample of matching messages
   - actually create a filter
-}


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
  forM_ returnKeyMeansNext (flip UI.onActivate (\x -> UI.focusNext filterFg))

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
       modifyIORef filtersRef (filt:)
       refreshMessages
       switchToMain
     Left e -> return ()

  filterDialog `UI.onDialogCancel` \_ -> do
    switchToMain

  return (filterCreationWindow, nameEdit, filterFg, filterDialog)


type IsPinned = Bool
type IsActive = Bool


-- TODO: Is it okay to use C8.unpack?
jsonToText = (T.pack . C8.unpack . Aeson.encode)

main = do

  -- examples
  Right getMessage <- return $ getPath "$['message']"
  Right getOtterService <- return $ getPath "$['otter_service']"
  Right getOtterFacility <- return $ getPath "$['otter_facility']"

  let convergerFilter = Filter getOtterService (Equals "converger")
  let kazooFilter = Filter getOtterFacility (Equals "kazoo")
  let kazooSends = AllFilters [
        kazooFilter,
        (Filter getMessage (HasSubstring "Sending request"))]

  let errorsOrConverger = AnyFilter [
        convergerFilter,
        (Filter Yield (HasKey "exception_type"))]

  args <- getArgs
  content <- BS.readFile (args !! 0)
  let inputLines = BS.split 10 content -- the docs show `split '\n' content`,
                                       -- but that don't work!

  -- parsing stuff
  let inputJsons = (map Aeson.decode inputLines) :: [Maybe Aeson.Value]
  let messages = catMaybes inputJsons

  -- Global Messages and Filters

  messagesRef <- newIORef ([] :: [(IsPinned, Aeson.Value)])
  filtersRef <- newIORef ([] :: [Filter])

  -- UI stuff

  -- main window
  writeIORef messagesRef $ map (False,) messages
  writeIORef filtersRef [errorsOrConverger]
  (ui, messageList, filterList, refreshMessages) <- makeMainWindow messagesRef filtersRef

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

  messageDetailFg `UI.onKeyPressed` \_ key _ ->
    case key of
     Events.KEsc -> switchToMain >> return True
     _ -> return False

  UI.runUi c UI.defaultContext
