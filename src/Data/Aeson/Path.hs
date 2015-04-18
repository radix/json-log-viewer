module Data.Aeson.Path where

import qualified Data.Aeson          as Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Text           as T
import qualified Data.Vector         as V

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

followPath :: JSONPath -> Aeson.Value -> Maybe Aeson.Value
followPath Yield value = Just value
followPath (Select (SelectKey key) remainingPath) (Aeson.Object obj)
  | Just value <- HM.lookup key obj = followPath remainingPath value
followPath (Select (SelectIndex index) remainingPath) (Aeson.Array array)
  | Just value <- array V.!? index = followPath remainingPath value
followPath _ _ = Nothing

