{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where

import qualified CMark
import           Control.Exception.Safe
import           Control.Monad          ((>=>))
import           Data.Text              (Text)
import qualified Data.Text              as Text
import qualified Data.Text.IO           as TIO
import qualified Data.Text.Lazy         as Lazy
import           Lib                    (Slug (..), frontmatterParser,
                                         makeIndexHtml)
import           Path                   (Dir, Path, Rel, reldir, relfile, (</>))
import qualified Path
import qualified System.Directory       as Dir
import qualified System.Directory       as Directory
import qualified System.IO              as IO
import           Text.Megaparsec

-- TODO: Maybe handle IO errors? Not sure with all the writeFile and readFile

blogPostDir :: Path Rel Dir
blogPostDir = [reldir|./content/blog/|]

data AppException
  = ExtractSlugE Text
  | UnknownException
  deriving (Show)

instance Exception AppException

-- | Extract information necessary to create the links to all posts in the
-- index.html file
extractSlugs ::
     Path Rel Dir
  -> IO (Either AppException Slug)
extractSlugs postsDir = do
  markdown <-
    readFile . Path.toFilePath $
    blogPostDir </> postsDir </> [relfile|./index.md|]
  case parse frontmatterParser "" markdown of
    Left e -> return . Left . ExtractSlugE . Text.pack $ show e
    Right v ->
      return . Right $
      Slug
        { _frontmatter = v
        , _postDir = Text.pack $ Path.toFilePath postsDir
        , _contentWithoutFrontmatter =
            Text.unlines . drop 4 . Text.lines $ Text.pack markdown
        }

main :: IO ()
main = do
  posts <- Directory.listDirectory $ Path.toFilePath blogPostDir
  -- ^ Each post is a directory
  sequence <$> traverse (Path.parseRelDir >=> extractSlugs) posts >>= \case
    Left e -> print e
    Right slugs -> do
      let outDir = [reldir|./public/|]
      Dir.createDirectoryIfMissing True $ Path.toFilePath outDir
      mapM_ (createAndWritePost outDir) slugs
      makeIndexHtml slugs >>=
        IO.writeFile (Path.toFilePath $ outDir </> [relfile|index.html|]) .
        Lazy.unpack
  where
    createAndWritePost dir slug =
      Path.parseRelDir (Text.unpack $ _postDir slug) >>= \postDir ->
        let html = CMark.commonmarkToHtml [] $ _contentWithoutFrontmatter slug
            fpath = Path.toFilePath $ dir </> postDir </> [relfile|index.html|]
         in Dir.createDirectoryIfMissing
              True
              (Path.toFilePath $ dir </> postDir) >>
            TIO.writeFile fpath html
