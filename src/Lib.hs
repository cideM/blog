{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Lib
  ( frontmatterParser
  , makeIndexHtml
  , Frontmatter
  , Slug(..)
  , extractSlugs
  ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Aeson             (defaultOptions, fieldLabelModifier,
                                         genericParseJSON, genericToJSON, (.=))
import qualified Data.Aeson             as Aeson
import qualified Data.ByteString.Char8  as Char8
import           Data.Text              (Text)
import qualified Data.Text              as Text
import qualified Data.Text.Lazy         as Lazy
import           Data.Void              (Void)
import           Data.Yaml              (FromJSON, ToJSON, decodeEither')
import           Exceptions
import           GHC.Generics
import           Path                   (Dir, Path, Rel, relfile, (</>))
import qualified Path
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Mustache          as Mustache

type Parser = Parsec Void String

data Frontmatter = Frontmatter
  { _title :: !Text
  , _date  :: !Text
  } deriving (Generic, Show)

-- All this genericParseJSON business is so aeson parses title but the
-- record field getter is _title
instance FromJSON Frontmatter where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 1}

instance ToJSON Frontmatter where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 1}

data Slug = Slug
  { _frontmatter               :: !Frontmatter
    -- ^ Contains title and date, delimited by ---
  , _postDir                   :: !Text
    -- ^ The name of the blog post (not the title from the frontmatter) is the
    -- name of the root folder holding index.md (and possibly assets)
  , _contentWithoutFrontmatter :: !Text
    -- ^ Content without first 4 lines
  } deriving (Generic, Show)

instance FromJSON Slug where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 1}

instance ToJSON Slug where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 1}

frontmatterParser :: Parser Frontmatter
frontmatterParser = do
  chars <- sep *> manyTill anySingle sep
  case decodeEither' $ Char8.pack chars of
    Left e  -> fail $ show e
    Right v -> return v
  where
    sep = string "---" >> eol

-- | Generate index.html with table of contents of links to individual posts
makeIndexHtml :: MonadIO m => [Slug] -> m Lazy.Text
makeIndexHtml posts =
  Mustache.compileMustacheFile "./src/index.mustache" >>= \template ->
    return $ Mustache.renderMustache template $ Aeson.object ["posts" .= posts]

-- | Extract information necessary to create the links to all posts in the
-- index.html file
extractSlugs ::
     MonadIO m => Path Rel Dir -> Path Rel Dir -> m (Either AppException Slug)
extractSlugs postsDir postDir = do
  markdown <-
    liftIO . readFile . Path.toFilePath $
    postsDir </> postDir </> [relfile|./index.md|]
  case parse frontmatterParser "" markdown of
    Left e -> return . Left . ExtractSlugE . Text.pack $ show e
    Right v ->
      return . Right $
      Slug
        { _frontmatter = v
        , _postDir = Text.pack $ Path.toFilePath postDir
        , _contentWithoutFrontmatter =
            Text.unlines . drop 4 . Text.lines $ Text.pack markdown
        }
