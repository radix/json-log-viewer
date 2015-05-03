{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}

module JsonLogViewer.FilterDialog
( FilterDialog
, makeFilterDialog
, filterFromDialog
, blankFilterDialog
, setDefaultsFromFilter) where

import           Control.Monad             (forM_, void)
import qualified Data.Aeson                as Aeson
import qualified Data.Aeson.Path.Parser    as Parser
import qualified Data.Text                 as T
import qualified Graphics.Vty.Input.Events as Events
import qualified Graphics.Vty.Widgets.All  as UI
import           JsonLogViewer.Filtration  (IsActive (..), JSONPredicate (..),
                                            LogFilter (..))
import           JsonLogViewer.UIUtils     (makeEditField, makeField)

-- |A bag of junk useful for filter creation/editing UIs
data FilterDialog = FilterDialog
  { nameEdit           :: UI.Widget UI.Edit
  , jsonPathEdit       :: UI.Widget UI.Edit
  , operandEdit        :: UI.Widget UI.Edit
  , operatorRadioGroup :: UI.RadioGroup
  , equalsCheck        :: UI.Widget (UI.CheckBox Bool)
  , substringCheck     :: UI.Widget (UI.CheckBox Bool)
  , hasKeyCheck        :: UI.Widget (UI.CheckBox Bool)
  }

makeFilterDialog
  :: T.Text -- ^ Name of the dialog
  -> IO () -- ^ action for switching away from the filter dialog when accepting/canceling
  -> IO (FilterDialog, UI.Dialog, UI.Widget UI.FocusGroup)
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

  let addFocus = void . UI.addToFocusGroup filterFg
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

  let fd = FilterDialog {
    nameEdit
  , jsonPathEdit
  , operandEdit
  , operatorRadioGroup
  , equalsCheck
  , substringCheck
  , hasKeyCheck
  }
  return (fd, filterDialog, filterFg)

-- | Create a Filter based on the contents of a filter-editing dialog.
filterFromDialog :: FilterDialog -> IO (Maybe LogFilter)
filterFromDialog fd = do
  pathText <- UI.getEditText (jsonPathEdit fd)
  operand <- UI.getEditText (operandEdit fd)
  currentRadio <- UI.getCurrentRadio (operatorRadioGroup fd)
  nameText <- UI.getEditText (nameEdit fd)
  let mpath = Parser.getPath $ T.unpack pathText
      predicate = case currentRadio of
        Just rButton
          | rButton == (equalsCheck fd) -> Equals $ Aeson.String operand
          | rButton == (substringCheck fd) -> HasSubstring operand
          | rButton == (hasKeyCheck fd) -> HasKey operand
        _ -> error "shouldn't happen because there's a default"

  case mpath of
   Right path -> do
     let filt = LogFilter {filterName=nameText,
                           jsonPath=path,
                           jsonPredicate=predicate,
                           filterIsActive=IsActive True}
     return $ Just filt
   Left _ -> return Nothing


blankFilterDialog :: FilterDialog -> IO ()
blankFilterDialog fd = do
  UI.focus (nameEdit fd)
  UI.setEditText (nameEdit fd) ""
  UI.setEditText (jsonPathEdit fd) ""
  UI.setCheckboxChecked (equalsCheck fd)
  UI.setEditText (operandEdit fd) ""


setDefaultsFromFilter :: FilterDialog -> LogFilter -> IO ()
setDefaultsFromFilter fd logFilter = do
  UI.focus (nameEdit fd)
  UI.setEditText (nameEdit fd) (filterName logFilter)
  UI.setEditText (jsonPathEdit fd) $ Parser.toString (jsonPath logFilter)
  case (jsonPredicate logFilter) of
   (Equals jsonVal) -> do
     UI.setCheckboxChecked (equalsCheck fd)
     case jsonVal of
      Aeson.String jsonText ->
        UI.setEditText (operandEdit fd) jsonText
      _ -> error "Only text is supported for json equality for now."
   (MatchesRegex _) -> error "MatchesRegex is not yet supported."
   (HasSubstring text) -> do
     UI.setCheckboxChecked (substringCheck fd)
     UI.setEditText (operandEdit fd) text
   (HasKey text) -> do
     UI.setCheckboxChecked (hasKeyCheck fd)
     UI.setEditText (operandEdit fd) text

