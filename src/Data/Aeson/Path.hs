module Data.Aeson.Path where

import qualified Data.Text as T
import qualified Data.Aeson as Aeson
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM

-- JSON PATH
-- TODO: Make this support all of JSONPath and split it off into an Aeson.Path
-- module.

data JSONSelector
  = SelectKey T.Text
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

