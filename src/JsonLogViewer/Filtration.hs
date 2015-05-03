{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE OverloadedStrings         #-}

module JsonLogViewer.Filtration where

import           Data.Aeson                ((.:), (.=))
import qualified Data.Aeson as Aeson
import           Data.Aeson.Path           (JSONPath (..), followPath)
import qualified Data.Text                 as T
import qualified Data.Vector               as V
import           Data.Foldable             (toList)
import           Control.Monad             (mzero)
import qualified Data.Aeson.Path.Parser    as Parser
import qualified Data.HashMap.Strict       as HM



newtype IsActive = IsActive {unIsActive :: Bool } deriving Show

data JSONPredicate
  = Equals Aeson.Value
  | MatchesRegex T.Text
  | HasSubstring T.Text
  | HasKey T.Text
  deriving Show

data LogFilter = LogFilter
  { jsonPath       :: JSONPath
  , jsonPredicate  :: JSONPredicate
  , filterName     :: T.Text
  , filterIsActive :: IsActive} deriving Show

lArray :: [Aeson.Value] -> Aeson.Value
lArray = Aeson.Array . V.fromList

instance Aeson.ToJSON JSONPredicate where
  toJSON (Equals jsonVal) = lArray [Aeson.String "Equals", jsonVal]
  toJSON (MatchesRegex text) = lArray [Aeson.String "MatchesRegex", Aeson.String text]
  toJSON (HasSubstring text) = lArray [Aeson.String "HasSubstring", Aeson.String text]
  toJSON (HasKey text) = lArray [Aeson.String "HasKey", Aeson.String text]

instance Aeson.FromJSON JSONPredicate where
  parseJSON (Aeson.Array v) = case toList v of
    [Aeson.String "Equals", val] -> pure $ Equals val
    [Aeson.String "MatchesRegex", Aeson.String text] -> pure $ MatchesRegex text
    [Aeson.String "HasSubstring", Aeson.String text] -> pure $ HasSubstring text
    [Aeson.String "HasKey", Aeson.String text] -> pure $ HasKey text
    _ -> mzero
  parseJSON _ = mzero

instance Aeson.ToJSON LogFilter where
  toJSON (LogFilter {..}) = Aeson.object [
    "name" .= filterName
    , "is_active" .= unIsActive filterIsActive
    , "path" .= Parser.toString jsonPath
    , "predicate" .= Aeson.toJSON jsonPredicate]

instance Aeson.FromJSON LogFilter where
  parseJSON (Aeson.Object o) = do
    -- I'd *like* to do this applicatively, but I'm not actually sure it's
    -- possible at all, given the Either handling below.
    name <- o .: "name"
    predicate <- o .: "predicate"
    pathText <- o .: "path"
    isActive <- o .: "is_active"
    let parsed = Parser.getPath pathText
    case parsed of
     Right path -> return LogFilter {filterName=name,
                                     filterIsActive=IsActive isActive,
                                     jsonPredicate=predicate, jsonPath=path}
     Left e -> fail ("Error when parsing jsonPath: " ++ show e)
  parseJSON _ = mzero

matchPredicate :: JSONPredicate -> Aeson.Value -> Bool
matchPredicate (Equals expected)       got = expected == got
matchPredicate (HasSubstring expected) (Aeson.String got) = expected `T.isInfixOf` got
matchPredicate (MatchesRegex _) _ = error "Implement MatchesRegex"
matchPredicate (HasKey expected) (Aeson.Object hm) = HM.member expected hm
matchPredicate _ _ = False

matchFilter :: LogFilter -> Aeson.Value -> Bool
matchFilter (LogFilter {jsonPath, jsonPredicate}) aesonValue
  | Just gotValue <- followPath jsonPath aesonValue
    = jsonPredicate `matchPredicate` gotValue
matchFilter _ _ = False


