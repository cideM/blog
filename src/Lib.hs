{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( frontmatterParser
  , makeIndex
  , Frontmatter
  , TOCPost
  ) where

import           Control.Monad.IO.Class (MonadIO)
import           Data.Aeson             ((.=))
import qualified Data.Aeson             as Aeson
import qualified Data.ByteString.Char8  as Char8
import           Data.Text              (Text)
import qualified Data.Text.Lazy         as Lazy
import           Data.Void              (Void)
import           Data.Yaml              (FromJSON, ToJSON, decodeEither')
import           GHC.Generics
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Mustache          as Mustache

type Parser = Parsec Void String

data Frontmatter = Frontmatter
  { title :: !Text
  , date  :: !Text
  } deriving (Generic, Show)

instance FromJSON Frontmatter

instance ToJSON Frontmatter

data TOCPost = TOCPost
  { frontmatter :: !Frontmatter
  , filename    :: !Text
  } deriving (Generic, Show)

instance FromJSON TOCPost

instance ToJSON TOCPost

{-
---
title: Foo
date: Something
---
gives Frontmatter { title: "Foo", date: "Something" }
-}
frontmatterParser :: Parser Frontmatter
frontmatterParser = do
  chars <- sep *> manyTill anySingle sep
  case decodeEither' $ Char8.pack chars of
    Left e  -> fail $ show e
    Right v -> return v
  where
    sep = string "---" >> eol

-- | Generate index.html with table of contents of links to individual posts
makeIndex :: MonadIO m => [TOCPost] -> m Lazy.Text
makeIndex posts =
  Mustache.compileMustacheFile "./src/index.mustache" >>= \template ->
    return $ Mustache.renderMustache template $ Aeson.object ["posts" .= posts]
