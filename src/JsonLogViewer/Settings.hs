{-# LANGUAGE OverloadedStrings #-}

module JsonLogViewer.Settings
( getSettingsFilePath
, writeSettingsFile
, loadSettingsFile
) where

import           Control.Exception        (tryJust)
import           Control.Monad            (guard, liftM)
import           Data.Aeson               as Aeson
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.Aeson.Types         as Aeson.Types
import qualified Data.ByteString.Lazy     as BSL
import           Data.Maybe               (fromMaybe)
import qualified Data.Text                as T
import           System.Directory         (createDirectoryIfMissing,
                                           getHomeDirectory)
import           System.FilePath          (splitFileName, (</>))
import           System.IO.Error          (isDoesNotExistError)

import           JsonLogViewer.Filtration (LogFilter)


getSettingsFilePath :: IO FilePath
getSettingsFilePath =
  getHomeDirectory `plus` (</> ".config"
                          </> "json-log-viewer"
                          </> "settings.json")
  where plus l r = liftM r l

writeSettingsFile :: FilePath -> [LogFilter] -> [T.Text] -> IO ()
writeSettingsFile fp filters columns = do
  let jsonBytes = encodePretty $ Aeson.object ["filters" .= filters,
                                               "columns" .= columns]
  createDirectoryIfMissing True $ fst $ splitFileName fp
  BSL.writeFile fp jsonBytes

parseSettings :: BSL.ByteString -> ([LogFilter], [T.Text])
parseSettings bytes =
  let columnsAndFilters = do -- maybe monad
        result <- Aeson.decode bytes
        flip Aeson.Types.parseMaybe result $ \obj -> do -- parser monad
          columns <- obj .: "columns"
          filters <- obj .: "filters"
          return (filters, columns)
  in
   fromMaybe ([], []) columnsAndFilters

loadSettingsFile :: FilePath -> IO ([LogFilter], [T.Text])
loadSettingsFile fp = do
  e <- tryJust (guard . isDoesNotExistError) (BSL.readFile fp)
  case e of
   Left _ -> return ([], [])
   Right bytes -> return $ parseSettings bytes

