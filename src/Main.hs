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
  = Equals Value
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
- dialog box for creating a filter
- tail a file, continuously apply filters (maybe revert to plain text for a bit
  to PoC this)
  - https://gist.github.com/ijt/1055731
- pinning lol

-}


main = do

  -- examples
  let getMessageE = getPath "$['message']"
  putStrLn $ show getMessageE
  Right getMessage <- return getMessageE
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

