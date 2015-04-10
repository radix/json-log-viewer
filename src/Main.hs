{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}

module Main where

import System.Environment (getArgs)
import Data.Aeson as Aeson
import Data.Maybe
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as Text
import qualified Data.Vector as V

import qualified Graphics.Vty.Widgets.All as UI
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as C8


-- JSON PATH
data JSONSelector
  = SelectKey Text.Text
  | SelectIndex Int

data JSONPath
  = Yield
  | Select JSONSelector JSONPath

jsonPath :: JSONPath -> Aeson.Value -> Maybe Aeson.Value
jsonPath Yield value = Just value
jsonPath (Select (SelectKey key) remainingPath) (Aeson.Object obj)
  | Just value <- HM.lookup key obj
    = jsonPath remainingPath value
jsonPath (Select (SelectIndex index) remainingPath) (Aeson.Array array)
  | Just value <- array V.!? index
    = jsonPath remainingPath value
jsonPath _ _ = Nothing


-- FILTRATION

data JSONPredicate
  = Equals Value
  | MatchesRegex T.Text
  | HasSubstring T.Text
  | HasKey T.Text

data Filter
  = Filter JSONPath JSONPredicate
  | AllFilters [Filter]
  | AnyFilter [Filter]

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
matchFilter (AllFilters filters) aesonValue = all (\f -> matchFilter f aesonValue) filters
matchFilter (AnyFilter filters) aesonValue = any (\f -> matchFilter f aesonValue) filters
matchFilter _ _ = False


-- sample
getMessage :: JSONPath
getMessage = Select (SelectKey "message") Yield

getOtterService :: JSONPath
getOtterService = Select (SelectKey "otter_service") Yield

getOtterFacility :: JSONPath
getOtterFacility = Select (SelectKey "otter_facility") Yield

convergerFilter :: Filter
convergerFilter = Filter getOtterService (Equals "converger")

kazooFilter :: Filter
kazooFilter = Filter getOtterFacility (Equals "kazoo")

kazooSends :: Filter
kazooSends = AllFilters [
  kazooFilter,
  (Filter getMessage (HasSubstring "Sending request"))]

errorsOrConverger :: Filter
errorsOrConverger = AnyFilter [
  convergerFilter,
  (Filter Yield (HasKey "exception_type"))]



main = do

  args <- getArgs
  content <- BS.readFile (args !! 0)
  let inputLines = BS.split 10 content -- the docs show you can do `split '\n'
                                       -- content`, but that don't work.

  -- parsing stuff
  let inputJsons = (map Aeson.decode inputLines) :: [Maybe Aeson.Value]
  let actualJsons = catMaybes inputJsons
  let messages = catMaybes $ map (jsonPath getMessage) actualJsons
  let filteredMessages = filter (matchFilter errorsOrConverger) actualJsons
  let filteredMessagesText = map (T.pack . C8.unpack . Aeson.encode) filteredMessages
  return "okay."

  -- UI stuff
  label <- UI.plainText "Enter your name:"
  e1 <- UI.editWidget
  labelAndEdit <- UI.hBox label e1
  prompt <- UI.bordered labelAndEdit
  messageList <- UI.newTextList filteredMessagesText 1
  promptAndList <- UI.vBox prompt messageList
  ui <- UI.centered promptAndList

  fg <- UI.newFocusGroup
  UI.addToFocusGroup fg e1
  UI.addToFocusGroup fg messageList

  c <- UI.newCollection
  UI.addToCollection c ui fg

  e1 `UI.onActivate` \this ->
    UI.getEditText this >>= (error . ("You entered: " ++) . T.unpack)

  UI.runUi c UI.defaultContext

