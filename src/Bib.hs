{-# LANGUAGE OverloadedStrings #-}
module Bib where

import Control.Applicative
import qualified Data.Text as T
import Data.Attoparsec.Text
import Data.Char
import Data.Maybe

------------------------------------
-- Model

data BibEntry = MkBibEntry {id :: T.Text, author :: T.Text, title :: T.Text}
  deriving (Eq, Ord, Show)

prettyPrint :: BibEntry -> String
prettyPrint (MkBibEntry fId fAuthor fTitle) = T.unpack fTitle ++ " • " ++ T.unpack fAuthor ++ " • (" ++ T.unpack fId ++ ")"


pBib :: Parser [BibEntry]
pBib = many pBibEntry

------------------------------------
-- Parsing bits and bobs
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
  v <- pCurlySur (skipSpace *> scan 0 f <* skipSpace)
  skipSpace
  pure (k , v)
  where
    f :: Int -> Char -> Maybe Int
    f n c | c == '{' = Just (n + 1)
          | c == '}' = if n == 0 then Nothing else Just (n - 1)
          | otherwise = Just n

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

pList' :: Parser a -> Parser [Maybe a]
pList' p = do
  mHead <- choice [pJust p, pNothing skipSpace]
  mTail <- choice [(pNothing (char ',') *> pNothing skipSpace *> pJust (pList' p))
                  , pNothing skipSpace]
  mxs <- pure $ case mTail of
    Nothing -> [ mHead ]
    Just xs -> mHead : xs
  pure mxs

pList :: Parser a -> Parser [a]
pList p = do
  mxs <- pList' p
  return $ catMaybes mxs

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

pLast :: Parser T.Text
pLast = do
  char '('
  x <- takeTill (== ')')
  char ')'
  endOfInput
  return x

pOut :: Parser T.Text
pOut = do
  skipWhile (/= '(')
  x <- choice [pLast, pOut]
  return x
