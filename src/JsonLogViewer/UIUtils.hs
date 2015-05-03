module JsonLogViewer.UIUtils where

import qualified Data.Text                as T
import qualified Graphics.Vty.Widgets.All as UI

makeField :: (Show a) => T.Text -> UI.Widget a -> IO (UI.Widget (UI.Box UI.FormattedText a))
makeField labelText widget = do
  label <- UI.plainText labelText
  UI.hBox label widget

makeEditField :: T.Text -> IO (UI.Widget UI.Edit, UI.Widget (UI.Box UI.FormattedText UI.Edit))
makeEditField labelText = do
  edit <- UI.editWidget
  field <- makeField labelText edit
  return (edit, field)
