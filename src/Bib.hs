{-# LANGUAGE ScopedTypeVariables #-}
module Bib where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Except
import Data.Functor

import Text.Parsec
import Text.Parsec.String
import qualified Text.BibTeX.Entry as BP
import qualified Text.BibTeX.Parse as BP
import Control.Monad.Trans.Except (ExceptT(ExceptT), Except, withExceptT)

------------------------------------
-- Types

data BibEntry = Mk {identifier :: String, author :: String, title :: String}
  deriving (Eq, Show)

-- Make sure we're sorting bibentries by id first and foremost
instance Ord BibEntry where
  compare (Mk id1 author1 title1) (Mk id2 author2 title2)
    | id1 == id2 = compare (author1, title1) (author2, title2) -- if the ID's are equal, then revert to the default comparison on the other fields
    | otherwise = compare id1 id2 -- otherwise sort according to id

data BibError = MkParseError ParseError | MkIOError IOError | MkFieldError String
  deriving (Eq, Show)

-- type alias for our bib parser monad
type BibM = ExceptT BibError IO


----------------------------------------------
-- API Functions

-- Render a bib entry as a string, for use in populating the fzf output
prettyPrint :: BibEntry -> String
prettyPrint (Mk fId fAuthor fTitle) = fTitle ++ " • " ++ fAuthor ++ " • (" ++ fId ++ ")"

-- Parse the bib file!
parseBib :: FilePath -> BibM [BibEntry]
parseBib = mapParseError . parseFromFile (BP.skippingLeadingSpace BP.file)

-- fzf will return a result in the form of a prettyPrinted string; here's how we extract the id
-- (and hence pdf file name) from that
parseResult :: String -> BibM String
parseResult input =
  let res = parse fzfResultParser "" input :: Either ParseError String
  in withExceptT MkParseError (liftEither res)


--------------------------------------------
-- Error Plumbing

exceptMaybe :: Monad m => e -> Maybe a -> ExceptT e m a
exceptMaybe err = maybe (throwError err) pure

-- the parser gives back an IO action that returns an either. here's how to wrap that as an ExceptT
liftMEither :: forall m e b. (Monad m) => m (Either e b) -> ExceptT e m b
liftMEither x =
  let a = (x <&> liftEither) :: m (ExceptT e m b)
      b = lift a             :: ExceptT e m (ExceptT e m b)
  in join b

-- Convert the T from the library to my BibEntry, and do some validation of the present fields in the process
fromT :: Monad m => BP.T -> ExceptT BibError m BibEntry
fromT (BP.Cons entryType identifier fields) = do
  author <- exceptMaybe (MkFieldError $ "'author' field is missing from bib entry '" ++ identifier ++ "'.") (lookup "author" fields)
  title <- exceptMaybe (MkFieldError $ "'title' field is missing from bib entry '" ++ identifier ++ "'.") (lookup "title" fields)
  pure (Mk identifier author title)

-- Actually convert the result of the parser to the type we need
mapParseError :: IO (Either ParseError [BP.T]) -> BibM [BibEntry]
mapParseError x = do
  ts <- withExceptT MkParseError (liftMEither x)
  mapM fromT ts

-------------------------------------------------
-- Pretty Printed Bib Parsing

-- the identifier lives inside brackets right before the end of the line
fzfResultParser :: Parser String
fzfResultParser = do
  skipMany anyChar
  char '('
  manyTill anyChar (char ')' >> eof)
