{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Attoparsec.Text
import System.Environment
import System.Process
import System.IO

import Bib

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

parseExact :: Parser a -> T.Text -> IO a
parseExact p txt = case parseOnly (p <* skipSpace <* endOfInput) txt of
    Left err -> ioError $ userError $ "Parse error: " ++ err
    Right bs -> pure bs

readAndParseBib :: IO [BibEntry]
readAndParseBib = do
  rawtxt <- tryReadBib
  parseExact pBib rawtxt




------------------------------------
-- Main

main :: IO ()
main = do
  args <- getArgs
  run args

run :: [String] -> IO ()
run [] = runHelp
run ("help" : _) = runHelp
run ("config" : _) = runConfig
run ("validate" : _) = do
  bs <- readAndParseBib
  runValidate bs
run ("fzf" : _) = do
  bs <- readAndParseBib
  runFzf bs
run (c : _) = putStrLn ("'" ++ c ++ "' is not a recognised command. Try 'fzfbiblio help'")

-- Print the help string to terminal.
runHelp :: IO ()
runHelp = putStrLn $
  "Available commands:\n\
  \  fzfbiblio help --- display this help text.\n\
  \  fzfbiblio config --- display the currently set config options.\n\
  \  fzfbiblio validate --- report bib entries without matching pdf file, and vice-versa.\n\
  \  fzfbiblio fzf --- invoke fzf to open a pdf file from the bib library.\n\
  \\n\
  \You must set the following environment variables, eg from your `.bashrc`:\n\
  \  `$FZFBIBLIO_BIB_FILE`, eg to `$HOME/bib/global.bib`. This must point to a bibtex file.\n\
  \  `$FZFBIBLIO_PDF_FOLDER`, eg to `$HOME/bib/pdf`\n\
  \  `$FZFBIBLIO_PDF_VIEWER`, eg to `zathura`"


-- Print the current config values and where they are sourced from (commandline, config file, env vars) to terminal
runConfig :: IO ()
runConfig = putStrLn "That feature is currently unimplemented, sorry!"

-- Parse the bib file, and compare to the files in the pdf folder. Return a report of files without matching bib entries
-- and vice-versa.
runValidate :: [BibEntry] -> IO ()
runValidate bs = do
  pdfs <- _ -- get the list of pdf file names, and chop off the file extensions
  (bs', pdfs') <- validate (sort bs) (sort pdfs) -- warning: sort may not behave as expected on BibEntry's!
  putStrLn "The following bib entries do not have an accompanying pdf file:"
  print bs'
  putStrLn "The following pdf files do not have an accompanying bib entry:"
  print pdfs'

-- returns the orphan bibentries (left) and pdf files (right)
validate :: [BibEntry] -> [String] -> ([String], [String])
validate = undefined

runFzf :: [BibEntry] -> IO ()
runFzf bs = do
  input_lines <- pure $ map prettyPrint bs

  (Just hIn, Just hOut, _, ph) <-
      createProcess (proc fzf_command fzf_args) { std_in = CreatePipe
                                                , std_out = CreatePipe
                                                , std_err = Inherit
                                                , delegate_ctlc = True }
  hSetBuffering hIn NoBuffering

-- if a significant delay becomes noticeable
-- at startup before the list becomes populated, and the bibfile also
-- happens to be large, then it may be because of mapM loading the whole
-- bib into memory before processing it. At that point, a streaming approach
-- using the pipes library would be better. Or, the bottleneck may be the
-- parser.
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
