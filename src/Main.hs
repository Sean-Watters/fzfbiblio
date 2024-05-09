{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Maybe
import Data.Char
import Data.Attoparsec.Text
import System.Environment
import Debug.Trace

------------------------------------
-- Environment variables

evBibLoc :: String
evBibLoc = "FZFBIBLIO_BIB_FILE"

evPdfLoc :: String
evPdfLoc = "FZFBIBLIO_PDF_FOLDER"

test :: T.Text
test = "@InProceedings{dybjersetzer99,\n  author    = {Dybjer, Peter and Setzer, Anton},\n  title     = {A Finite Axiomatization of Inductive-Recursive Definitions},\n  editor    = {Girard, Jean-Yves},\n  booktitle = {Typed Lambda Calculi and Applications},\n  year      = {1999},\n  publisher = {Springer},\n  pages     = {129--146},\n}\n"


------------------------------------
-- IO

tryReadBib :: IO T.Text
tryReadBib = do
  path <- getEnv evBibLoc
  T.readFile path



------------------------------------
-- Model

data BibEntry = MkBibEntry {id :: T.Text, author :: T.Text, title :: T.Text}
  deriving (Eq, Ord, Show)

------------------------------------
-- Parsing
isSpaceOrChar :: Char -> Char -> Bool
isSpaceOrChar x c = isSpace c || c == x

pCurlySur :: Parser a -> Parser a
pCurlySur p = do
  char '{'
  skipSpace
  a <- p
  skipSpace
  char '}'
  return a

pPair :: Parser (T.Text, T.Text)
pPair = do
  skipSpace
  k <- takeTill (isSpaceOrChar '=')
  skipSpace
  char '='
  skipSpace
  v <- pCurlySur (skipSpace *> takeTill (== '}') <* skipSpace)
  skipSpace
  pure (k , v)

pMaybe :: Parser a -> Parser (Maybe a)
pMaybe p = do
  mx <- eitherP p skipSpace
  case mx of
    Left x -> return $ Just x
    Right () -> return $ Nothing

pNothing :: Parser a -> Parser (Maybe b)
pNothing p = do
  _ <- p
  pure Nothing

pJust :: Parser a -> Parser (Maybe a)
pJust p = do
  x <- p
  pure (Just x)

-- match on text of alternating A's and B's, of any parity
-- Like sepBy, but it slurps input left to right rather (unlike `sepBy` (char ','), which also sl)
pList' :: Parser a -> Parser [Maybe a]
pList' p = do
  mHead <- choice [pJust p, pNothing skipSpace]
  mTail <- choice [(pNothing (char ',') *> pNothing skipSpace *> pJust (pList' p))
                  , pNothing skipSpace]
  mxs <- pure $ case mTail of
    Nothing -> [ mHead ]
    Just xs -> mHead : xs
  pure mxs

-- parser for a comma-separated list of a's, which does *not* use
-- sepBy, as that captures commas inside our a chunks, which is bad.
-- chunks consisting only of whitespace are ignored in the output list
pList :: Parser a -> Parser [a]
pList p = do
  mxs <- pList' p
  -- mxs <- many (pMaybe p <* skipSpace <* char ',')
  return $ catMaybes mxs

-- the stuff inside the curly brackets
pBibEntryData :: Parser BibEntry
pBibEntryData = do
  fId <- takeTill (isSpaceOrChar ',')
  skipSpace
  char ','
  skipSpace
  entries <- pList pPair
  ----------
  mAuthor <- return $ lookup "author" entries
  mTitle <- return $ lookup "title" entries
  fTitle <- case isJust mTitle of
             True -> return $ fromJust mTitle
             False -> fail "no title field"
  fAuthor <- case isJust mAuthor of
             True -> return $ fromJust mAuthor
             False -> fail "no author field"
  return $ MkBibEntry fId fAuthor fTitle

pBibEntry :: Parser BibEntry
pBibEntry = do
  skipSpace
  takeTill (isSpaceOrChar '{')
  skipSpace
  x <- (pCurlySur pBibEntryData)
  skipSpace
  return x

pBib :: Parser [BibEntry]
pBib = many (pBibEntry) <* skipSpace

------------------------------------
-- Main

parseExact :: Parser a -> T.Text -> Either String a
parseExact p = parseOnly (p <* skipSpace <* endOfInput)

main :: IO ()
main = do
  rawtxt <- tryReadBib
  bs <- pure $ parseExact pBib rawtxt
  trace (show bs) (pure ())
  return ()

-- TODO - add functionality for checking for bib entries without a pdf, and pdfs without bib entries
