module Main where

import Data.Text
import Data.Maybe
import Data.Char
import Data.Attoparsec.Text
import Data.Attoparsec.Combinator
import System.Environment

------------------------------------
-- Environment variables

evBibLoc :: String
evBibLoc = "FZFBIBLIO_BIB_FILE"

evPdfLoc :: String
evPdfLoc = "FZFBIBLIO_PDF_FOLDER"

------------------------------------
-- IO

tryReadBib :: IO Text
tryReadBib = do
  path <- lookupEnv evBibLoc
  readFile path



------------------------------------
-- Model

data BibEntry = MkBibEntry {id :: String, author :: String, title :: String}

------------------------------------
-- Parsing

pPair :: Parser (String, String)
pPair = do
  skipSpace
  k <- takeTill isSpace
  skipSpace
  char '='
  skipSpace
  char '{'
  skipSpace
  v <- takeTill (== '}')
  char '}'
  pure (k , v)

pBibEntry :: Parser BibEntry
pBibEntry = do
  skipSpace
  takeTill (== '{')
  char '{'
  skipSpace
  id <- takeTill isSpace
  skipSpace
  entries <- manyTill (pPair *> char ',' *> skipSpace) (char '}')
  mAuthor <- lookup "author" entries
  mTitle <- lookup "title" entries
  title <- case isJust mTitle of
             True -> fromJust mTitle
             False -> fail "no title field"
  author <- case isJust mAuthor of
             True -> fromJust mAuthor
             False -> fail "no author field"
  return $ MkBibEntry id author title

pBib :: Parser [BibEntry]
pBib = undefined

------------------------------------
-- Main

main :: IO ()
main = do
  rawtxt <- tryReadBib
  pure $ parse pBib rawtxt

-- TODO - add functionality for checking for bib entries without a pdf, and pdfs without bib entries
