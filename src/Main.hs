{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}

module Main where

import System.Environment (getArgs)
import qualified System.Exit as Exit

import Data.Maybe
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Vector as V

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as AesonPretty

import qualified Graphics.Vty.Widgets.All as UI
import qualified Graphics.Vty.Input.Events as Events
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as C8

import qualified Text.Parsec as P


-- JSON PATH
-- TODO: Make this support all of JSONPath and split it off into an Aeson.Path
-- module.

data JSONSelector
  = SelectKey Text.Text
  | SelectIndex Int
  deriving Show

data JSONPath
  = Yield
  | Select JSONSelector JSONPath
  deriving Show

jsonPath :: JSONPath -> Aeson.Value -> Maybe Aeson.Value
jsonPath Yield value = Just value
jsonPath (Select (SelectKey key) remainingPath) (Aeson.Object obj)
  | Just value <- HM.lookup key obj = jsonPath remainingPath value
jsonPath (Select (SelectIndex index) remainingPath) (Aeson.Array array)
  | Just value <- array V.!? index = jsonPath remainingPath value
jsonPath _ _ = Nothing


-- Parsing of JSON Path
-- TODO: Make this support all of JSONPath and split it off into an
-- Aeson.Path.Parser module.

{-
examples:
- $['message']
- $['foobars'][1]
-}

-- jsonPathParser is a Parsec parser that parses a String and produces a
-- JSONPath.
-- operator cheatsheet:
-- - '*>' parse the left then parse the right, and return the right.
-- - '<*' same, but return the left.
-- - '<*>' I haven't internalized this one yet, in the context of parsec :(
-- - '<|>' either parse the left or the right.
-- - '<$>' apply the pure function on the left to the result of parsing the
--   right.
jsonPathParser :: P.Parsec String st JSONPath
jsonPathParser = dollar *> pathItems
  where
    dollar           = P.char '$'
    quote            = P.char '\''
    number           = read <$> (P.many1 $ P.oneOf "0123456789")
    str              = (P.many $ P.noneOf "'")
    quotedAttribute  = P.between quote quote str
    selectIndex      = SelectIndex <$> number
    selectKey        = (SelectKey . T.pack) <$> quotedAttribute
    selector         = selectIndex P.<|> selectKey
    selectorBrackets = P.between (P.char '[') (P.char ']') selector

    -- The fun/tricky bit: `select` results in a *partially applied* `Select`,
    -- and it is filled in with the remaining JSONPath in the recursive
    -- `pathItems`.
    yield            = (const Yield) <$> P.eof
    select           = Select <$> selectorBrackets
    pathItems        = yield P.<|> (select <*> pathItems)

getPath :: String -> Either P.ParseError JSONPath
getPath st = P.parse jsonPathParser "" st


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

  -- message detail
  mdHeader <- UI.plainText "Message Detail. ESC=return"
  mdHeader <- UI.bordered mdHeader
  mdBody <- UI.plainText "Insert message here."
  messageDetail <- UI.vBox mdHeader mdBody

  mainFg <- UI.newFocusGroup
  UI.addToFocusGroup mainFg messageList
  UI.addToFocusGroup mainFg filterList

  messageDetailFg <- UI.newFocusGroup

  c <- UI.newCollection
  switchToMain <- UI.addToCollection c ui mainFg
  switchToMessageDetail <- UI.addToCollection c messageDetail messageDetailFg

  mainFg `UI.onKeyPressed` \_ key _ ->
    case key of
     Events.KEsc -> Exit.exitSuccess
     _ -> return False

  messageList `UI.onItemActivated` \(UI.ActivateItemEvent _ s _) -> do
    let decoded = (Aeson.decode (C8.pack (T.unpack s)) :: Maybe Aeson.Value)
    Just decoded <- return decoded
    UI.setText mdBody $ T.pack $ C8.unpack $ AesonPretty.encodePretty decoded
    switchToMessageDetail

  messageDetailFg `UI.onKeyPressed` \_ key _ ->
    case key of
     Events.KEsc -> switchToMain >> return True
     _ -> return False

  UI.runUi c UI.defaultContext

