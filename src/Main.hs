module Main where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Except
import System.Environment
import System.Process
import System.IO
import System.FilePath
import System.Directory

import Bib
import System.FilePath (dropExtension)
import GHC.IO.SubSystem (withIoSubSystem)

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

-- TODO: can set env vars for the scope of this process. may be an easy way to incorporate config values without
-- changing code

------------------------------------
-- IO

launchPdfViewer :: FilePath -> IO ()
launchPdfViewer p = do
  cmd <- getEnv evPdfViewer
  callProcess cmd [p]

runBibParser :: BibM [BibEntry]
runBibParser = do
  path <- liftIO $ getEnv evBibLoc
  parseBib path

------------------------------------
-- Output Parsing

--

------------------------------------
-- Main

main :: IO ()
main = do
  args <- getArgs
  runExceptT $ run args
  return ()

run :: [String] -> BibM ()
run [] = liftIO runHelp
run ("help" : _) = liftIO runHelp
run ("config" : _) = liftIO runConfig
run ("validate" : _) = do
  bs <- runBibParser
  liftIO $ runValidate bs
  return ()
run ("fzf" : _) = do
  bs <- runBibParser
  liftIO $ runFzf bs
  return ()
run (c : _) = liftIO $ putStrLn ("'" ++ c ++ "' is not a recognised command. Try 'fzfbiblio help'")

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
  -- get the list of pdf files in the pdfs directory, sans file extension
  pdfFolder <- getEnv evPdfLoc
  files <- listDirectory pdfFolder
  let pdfs = map dropExtension $ filter (\ z -> takeExtension z == ".pdf") files
  (bs', pdfs') <- pure $ validate (map Bib.identifier bs) pdfs
  putStrLn "The following bib entries do not have an accompanying pdf file:\n"
  print bs'
  putStrLn "\nThe following pdf files do not have an accompanying bib entry:\n"
  print pdfs'

-- returns the orphan bibentries (left) and pdf files (right)
-- speed is unlikely to be an issue, but if so can rewrite this to only do 1 traversal
validate :: [String] -> [String] -> ([String], [String])
validate bs ps = let bs' = filter (`elem` ps) bs
                     ps' = filter (`elem` bs) ps
                 in (bs', ps')

openPdfFile :: String -> BibM ()
openPdfFile res = do
  target <- parseResult res
  basepath <- liftIO $ getEnv evPdfLoc
  let fullpath = basepath ++ "/" ++ target ++ ".pdf"
  liftIO $ launchPdfViewer fullpath
  return ()

runFzf :: [BibEntry] -> IO ()
runFzf bs = do
  let input_lines = map prettyPrint bs

  -- fork an fzf process and keep IO handles
  (Just hIn, Just hOut, _, ph) <-
      createProcess (proc fzf_command fzf_args) { std_in = CreatePipe
                                                , std_out = CreatePipe
                                                , std_err = Inherit
                                                , delegate_ctlc = True }
  hSetBuffering hIn NoBuffering

-- Send the lines to the fzf process over the handle.
-- NB: mapM has load the whole bib into memory before processing it.
-- If a significant delay becomes noticeable at startup before the list
-- becomes populated, a streaming approach using the pipes library may be
-- better. Or, the bottleneck may be the parser.
  mapM (hPutStrLn hIn) input_lines
  waitForProcess ph

  output <- hGetLine hOut
  hClose hOut
  hClose hIn

  runExceptT $ openPdfFile output
  return ()
