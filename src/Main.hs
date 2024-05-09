{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Maybe
import Data.Char
import Data.Attoparsec.Text
import System.Environment
import System.Process
import System.IO

------------------------------------
-- Environment variables

evBibLoc :: String
evBibLoc = "FZFBIBLIO_BIB_FILE"

evPdfLoc :: String
evPdfLoc = "FZFBIBLIO_PDF_FOLDER"

evPdfViewer :: String
evPdfViewer = "FZFBIBLIO_PDF_VIEWER"

-- not environment vars, but maybe should be promoted to that?
fzf_command :: String
fzf_command = "fzf"

fzf_args :: [String]
fzf_args = ["--height", "90%"]
------------------------------------
-- IO

tryReadBib :: IO T.Text
tryReadBib = do
  path <- getEnv evBibLoc
  T.readFile path

launchPdfViewer :: FilePath -> IO ()
launchPdfViewer p = do
  cmd <- getEnv evPdfViewer
  callProcess cmd [p]


------------------------------------
-- Model

data BibEntry = MkBibEntry {id :: T.Text, author :: T.Text, title :: T.Text}
  deriving (Eq, Ord, Show)

prettyPrint :: BibEntry -> String
prettyPrint (MkBibEntry fId fAuthor fTitle) = T.unpack fTitle ++ " • " ++ T.unpack fAuthor ++ " • (" ++ T.unpack fId ++ ")"

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
pBib = many pBibEntry

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


------------------------------------
-- Main

parseExact :: Parser a -> T.Text -> IO a
parseExact p txt = case parseOnly (p <* skipSpace <* endOfInput) txt of
    Left err -> ioError $ userError $ "Parse error: " ++ err
    Right bs -> pure bs



main :: IO ()
main = do
  rawtxt <- tryReadBib
  bs <- parseExact pBib rawtxt
  input_lines <- pure $ map prettyPrint bs

  (Just hIn, Just hOut, _, ph) <-
      createProcess (proc fzf_command fzf_args) { std_in = CreatePipe
                                                , std_out = CreatePipe
                                                , std_err = Inherit
                                                , delegate_ctlc = True }

  hSetBuffering hIn NoBuffering
  mapM (hPutStrLn hIn) input_lines
  waitForProcess ph
  output <- hGetLine hOut
  hClose hOut
  hClose hIn

  target <- parseExact pOut (T.pack output)

  basepath <- getEnv evPdfLoc
  fullpath <- pure $ (basepath ++ "/" ++ T.unpack target ++ ".pdf")

  launchPdfViewer fullpath

  return ()

-- TODO - add functionality for checking which bib entries don't have a pdf,
-- and which pdfs don't have bib entries
