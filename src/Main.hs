{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}

module Main where

import System.Environment (getArgs)
import Data.Aeson as Aeson
import Data.Maybe
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as Text
import qualified Data.Vector as V

import Graphics.Vty.Widgets.All
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BS


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

data Filter
  = Filter JSONPath JSONPredicate
  | Filters [Filter]


matchFilter :: Filter -> Aeson.Value -> Maybe Aeson.Value
matchFilter (Filter jspath (Equals expected)) aesonValue
  | Just gotValue <- jsonPath jspath aesonValue
    = if gotValue == expected then
        Just aesonValue
      else
        Nothing
matchFilter _ _ = Nothing


-- sample
getMessage :: JSONPath
getMessage = Select (SelectKey "message") Yield

getOtterService :: JSONPath
getOtterService = Select (SelectKey "otter_service") Yield

convergerFilter :: Filter
convergerFilter = Filter getOtterService (Equals "converger")

main = do

  args <- getArgs
  content <- BS.readFile (args !! 0)
  let inputLines = BS.split 10 content -- the docs show you can do `split '\n'
                                       -- content`, but that don't work.

  -- parsing stuff
  let inputJsons = (map Aeson.decode inputLines) :: [Maybe Aeson.Value]
  let actualJsons = catMaybes inputJsons
  putStrLn ("parsed " ++ (show $ length inputJsons) ++ " lines of text")
  putStrLn ("got " ++ (show $ length $ actualJsons) ++ " actual jsons")
  let messages = catMaybes $ map (jsonPath getMessage) actualJsons
  let messagesWithConverger = catMaybes $ map (matchFilter convergerFilter) actualJsons
  mapM_ (putStrLn . show) messagesWithConverger
  return "okay."

  -- UI stuff
  label <- plainText "Enter your name:"
  e1 <- editWidget
  box <- hBox label e1
  borderedW <- bordered box
  ui <- centered borderedW

  fg <- newFocusGroup
  addToFocusGroup fg e1

  c <- newCollection
  addToCollection c ui fg

  e1 `onActivate` \this ->
    getEditText this >>= (error . ("You entered: " ++) . T.unpack)

  -- runUi c defaultContext

