{-# LANGUAGE OverloadedStrings         #-}

module Main where

import qualified Data.Text as T
import Control.Monad
import qualified Graphics.Vty.Input.Events  as Events
import qualified Graphics.Vty.Widgets.All   as UI
import           System.Exit                (exitSuccess)
import           System.Environment         (getArgs)


main = do
  messageList <- UI.newList 1
  args <- getArgs
  [tops] <- return args
  let top = (read tops) :: Int

  let adder x = do
        widg <- UI.plainText $ T.pack ("foo" ++ show x)
        UI.addToList messageList "unused" widg
  mapM_ adder [1..top]

  mainFg <- UI.newFocusGroup
  UI.addToFocusGroup mainFg messageList
  mainFg `UI.onKeyPressed` \_ key _ ->
    case key of
     Events.KEsc -> exitSuccess
     _ -> return False

  c <- UI.newCollection
  switchToMain <- UI.addToCollection c messageList mainFg
  UI.runUi c UI.defaultContext
