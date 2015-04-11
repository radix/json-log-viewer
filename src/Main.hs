{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PatternGuards             #-}

module Main where

import           Control.Monad              (forM_)
import qualified Data.Aeson                 as Aeson
import           Data.Aeson.Encode.Pretty   (encodePretty)
import qualified Data.ByteString.Lazy       as BS
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.HashMap.Strict        as HM
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
- dialog box for listing, activating/deactivating filters
- dialog box for creating a filter
- tail a file, continuously apply filters (maybe revert to plain text for a bit
  to PoC this)
  - https://gist.github.com/ijt/1055731
- filter persistence
- message inspection
- pinning lol

-}



makeMainWindow filteredMessagesText = do
  mainHeader <- UI.plainText "json-log-viewer by radix. ESC=Exit, TAB=Switch section, DEL=Delete filter, RET=Inspect, P=Pin message, C=Create Filter"
  borderedMainHeader <- UI.bordered mainHeader
  messageHeader <- UI.plainText "Messages"
  messageList <- UI.newTextList filteredMessagesText 1
  filterHeader <- UI.plainText "Filters"
  filterList <- UI.newTextList ["foo", "bar"] 1
  borderedFilters <- UI.bordered filterList
  borderedMessages <- UI.bordered messageList
  messagesAndHeader <- UI.vBox messageHeader borderedMessages
  filtersAndHeader <- UI.vBox filterHeader borderedFilters
  hb <- UI.hBox filtersAndHeader messagesAndHeader
  UI.setBoxChildSizePolicy hb $ UI.Percentage 15
  headerAndBody <- UI.vBox borderedMainHeader hb
  ui <- UI.centered headerAndBody
  return (ui, messageList, filterList)


makeMessageDetailWindow = do
  mdHeader <- UI.plainText "Message Detail. ESC=return"
  mdHeader <- UI.bordered mdHeader
  mdBody <- UI.plainText "Insert message here."
  messageDetail <- UI.vBox mdHeader mdBody
  return (messageDetail, mdBody)

makeFilterCreationWindow = do
  {-
   TODO:
   - sample of matching messages
   - actually create a filter
-}

  nameLabel <- UI.plainText "Filter Name:"
  nameEdit <- UI.editWidget
  nameField <- UI.hBox nameLabel nameEdit

  jsonPathLabel <- UI.plainText "JSON Path:"
  jsonPathEdit <- UI.editWidget
  jsonPathField <- UI.hBox jsonPathLabel jsonPathEdit

  parseStatusLabel <- UI.plainText "Parse status:"
  parseStatusText <- UI.plainText "Please Enter a JSON Path."
  parseStatusField <- UI.hBox parseStatusLabel parseStatusText

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

  operandLabel <- UI.plainText "Operand:"
  operandEdit <- UI.editWidget
  operandField <- UI.hBox operandLabel operandEdit

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

  return (filterCreationWindow, nameEdit, filterFg, filterDialog)

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
  let actualJsons = catMaybes inputJsons
  let messages = catMaybes $ map (jsonPath getMessage) actualJsons
  let filteredMessages = filter (matchFilter errorsOrConverger) actualJsons
  let filteredMessagesText = map (T.pack . C8.unpack . Aeson.encode) filteredMessages

  -- UI stuff

  -- main window
  (ui, messageList, filterList) <- makeMainWindow filteredMessagesText

  -- message detail window
  (messageDetail, mdBody) <- makeMessageDetailWindow

  -- filter creation window
  (filterCreationWindow, filterNameEdit, filterFg, filterDialog) <- makeFilterCreationWindow


  mainFg <- UI.newFocusGroup
  UI.addToFocusGroup mainFg messageList
  UI.addToFocusGroup mainFg filterList

  messageDetailFg <- UI.newFocusGroup

  c <- UI.newCollection
  switchToMain <- UI.addToCollection c ui mainFg
  switchToMessageDetail <- UI.addToCollection c messageDetail messageDetailFg
  switchToFilterCreation <- UI.addToCollection c filterCreationWindow filterFg

  mainFg `UI.onKeyPressed` \_ key _ ->
    case key of
     Events.KEsc -> exitSuccess
     (Events.KChar 'f') -> UI.focus filterNameEdit >> switchToFilterCreation >> return True
     _ -> return False

  messageList `UI.onItemActivated` \(UI.ActivateItemEvent _ s _) -> do
    let decoded = (Aeson.decode (C8.pack (T.unpack s)) :: Maybe Aeson.Value)
    let decodingError = "can't decode json even though I already did?"
    let prettyPrint = \json -> do
          T.pack $ C8.unpack $ encodePretty decoded
    UI.setText mdBody $ maybe decodingError prettyPrint decoded
    switchToMessageDetail

  messageDetailFg `UI.onKeyPressed` \_ key _ ->
    case key of
     Events.KEsc -> switchToMain >> return True
     _ -> return False

  filterDialog `UI.onDialogAccept` const switchToMain
  filterDialog `UI.onDialogCancel` const switchToMain

  UI.runUi c UI.defaultContext
