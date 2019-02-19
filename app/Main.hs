{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where

import qualified CMark
import           Control.Monad    ((>=>))
import qualified Data.Text        as Text
import qualified Data.Text.IO     as TIO
import qualified Data.Text.Lazy   as Lazy
import           Lib              (Slug (..), extractSlugs, makeIndexHtml)
import           Path             (Dir, Path, Rel, reldir, relfile, (</>))
import qualified Path
import qualified System.Directory as Dir
import qualified System.Directory as Directory
import qualified System.IO        as IO

-- TODO: Maybe handle IO errors? Not sure with all the writeFile and readFile
postsDir :: Path Rel Dir
postsDir = [reldir|./content/blog/|]

main :: IO ()
main = do
  posts <- Directory.listDirectory $ Path.toFilePath postsDir
  -- ^ Each post is a directory
  sequence <$> traverse (Path.parseRelDir >=> extractSlugs postsDir) posts >>= \case
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
