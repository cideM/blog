{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where

import           Control.Exception.Safe
import           Data.Void              (Void)
import           Lib
import           Path                   (Dir, Path, Rel)
import qualified Path
import qualified System.Directory       as Directory
import           Text.Megaparsec

contentDir :: Path Rel Dir
contentDir = [Path.reldir|./content/blog/|]

extractTOC ::
     FilePath -> IO (Either (ParseErrorBundle String Void) Lib.Frontmatter)
extractTOC dirname = do
  dirname' <- Path.parseRelDir dirname
  let mdFile = contentDir Path.</> dirname' Path.</> [Path.relfile|./index.md|]
  markdown <- readFile $ Path.fromRelFile mdFile
  return $ parse Lib.frontmatterParser "" markdown

main :: IO ()
main
    -- cwd <- Directory.getCurrentDirectory
 = do
  posts <- Directory.listDirectory $ Path.fromRelDir contentDir
  toc <- sequence <$> traverse extractTOC posts
  case toc of
    Left e  -> print e
    Right v -> print v
