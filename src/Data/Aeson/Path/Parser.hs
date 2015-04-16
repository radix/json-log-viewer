{-# LANGUAGE OverloadedStrings #-}

module Data.Aeson.Path.Parser where

import           Data.Aeson.Path
import           Data.Text       as T
import qualified Text.Parsec     as P


-- Parsing of JSON Path
-- TODO: Make this support all of JSONPath and split it off into an
-- Aeson.Path.Parser package.

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
    number           = read <$> P.many1 (P.oneOf "0123456789")
    str              = P.many $ P.noneOf "'"
    quotedAttribute  = P.between quote (quote P.<?> "single-quoted key name") str
    selectIndex      = SelectIndex <$> number
    selectKey        = (SelectKey . T.pack) <$> quotedAttribute
    selector         = (selectIndex P.<|> selectKey) P.<?> "integer or single-quoted string"
    selectorBrackets = P.between (P.char '[') (P.char ']') selector

    -- The fun/tricky bit: `select` results in a *partially applied* `Select`,
    -- and it is filled in with the remaining JSONPath in the recursive
    -- `pathItems`.
    yield            = const Yield <$> P.eof
    select           = Select <$> selectorBrackets
    pathItems        = yield P.<|> (select <*> pathItems)

getPath :: String -> Either P.ParseError JSONPath
getPath = P.parse jsonPathParser ""

toString :: JSONPath -> T.Text
toString x = "$" `T.append` go x
  where go Yield                             = ""
        go (Select selector path)     = "[" `T.append` (selecta selector) `T.append` "]" `T.append` go path
        selecta (SelectKey key) = "'" `T.append` key `T.append` "'"
        selecta (SelectIndex num) = T.pack $ show num
