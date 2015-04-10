{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)
import Data.Aeson as Aeson
import Data.Maybe
import qualified Data.Text as Text

import Graphics.Vty.Widgets.All
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BS


data JSONSelector
  = SelectKey Text.Text
  | SelectIndex Integer

data JSONPath = JSONPath [JSONSelector]

data JSONPredicate
  = MatchesValue Value
  | MatchesRegex String

data Filter
  = Filter JSONSelector JSONPredicate
  | Filters [Filter]

{-

TODO:

- data model for filters?

- pure functions for filtering
Value -> Filter -> Value

-}

main = do

  args <- getArgs
  content <- BS.readFile (args !! 0)
  let inputLines = BS.split 10 content -- the docs show you can do `split '\n'
                                       -- content`, but that don't work.
  
  -- parsing stuff
  let inputJsons = (map Aeson.decode inputLines) :: [Maybe Aeson.Value]
  putStrLn ("parsed " ++ (show $ length inputJsons) ++ " lines of text")
  putStrLn ("got " ++ (show $ length $ catMaybes inputJsons) ++ " actual jsons")
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

  runUi c defaultContext
