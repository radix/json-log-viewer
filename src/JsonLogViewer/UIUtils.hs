{-# LANGUAGE NamedWildCards        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}

module JsonLogViewer.UIUtils where

import qualified Data.Text                 as T
import qualified Graphics.Vty.Attributes   as Attrs
import qualified Graphics.Vty.Input.Events as Events
import           Graphics.Vty.Widgets.All  ((<++>), (<-->))
import qualified Graphics.Vty.Widgets.All  as UI


makeField :: (Show a) => T.Text -> UI.Widget a -> IO (UI.Widget (UI.Box UI.FormattedText a))
makeField labelText widget = do
  label <- UI.plainText labelText
  UI.hBox label widget

makeEditField :: T.Text -> IO (UI.Widget UI.Edit,
                               UI.Widget (UI.Box UI.FormattedText UI.Edit))
makeEditField labelText = do
  edit <- UI.editWidget
  field <- makeField labelText edit
  return (edit, field)

-- |Make a bordered list with header text and a "selected/total" label in the
-- bottom border. TODO: this looks ugly because it's not rendering corners.
-- This would be unnecessary if Bordered had a bottom label.
makeCoolList
  :: Show b
  => Int
  -> T.Text
  -> IO (UI.Widget (UI.List a b),
         UI.Widget _W)
makeCoolList itemSize label = do
  list <- UI.newList itemSize
  topBorder <- UI.hBorder >>= UI.withHBorderLabel label
  bottomBorder <- UI.hBorder >>= UI.withHBorderLabel "0/0"
  bordered <- return topBorder
              <-->
              (UI.vBorder <++> return list <++> UI.vBorder)
              <-->
              return bottomBorder

  let updateBottomLabel = do
        total <- UI.getListSize list
        selection <- UI.getSelected list
        let index = case selection of
              Just (idx, _) -> idx
              Nothing -> 0
        let txt = T.concat [T.pack $ show (index + 1), "/", T.pack $ show total]
        UI.setHBorderLabel bottomBorder txt

  list `UI.onSelectionChange` const updateBottomLabel
  list `UI.onItemAdded` const updateBottomLabel
  list `UI.setSelectedUnfocusedAttr` Just (Attrs.defAttr `Attrs.withStyle` Attrs.reverseVideo)

  list `UI.onKeyPressed` \_ key _ -> case key of
    Events.KChar 'j' -> do
      UI.scrollDown list
      return True
    Events.KChar 'k' -> do
      UI.scrollUp list
      return True
    _ -> return False

  return (list, bordered)
