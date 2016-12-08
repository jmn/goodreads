{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import qualified G
import Options.Applicative
import Types
import qualified Paths_g as Meta (version)
import Data.Version (showVersion)
import qualified Data.ByteString.Lazy.Char8 as L8
import Control.Exception (SomeException, try)

appOptions :: Parser AppOptions
appOptions = AppOptions
    <$> optional (strOption
        ( short 'k' <> long "with-key"
       <> metavar "APIKEY"
       <> help "Supply the API key as a command line argument."))

    <*> optional (option auto
        ( short 'l' <> long "limit"
       <> metavar "LIMIT"
       <> help "Limit the number of responses to LIMIT" ))
    
-- | Helper Function     
withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

parseOptions :: Parser Options
parseOptions = Options <$> appOptions <* version <*> parseCommand 

version :: Parser (a -> a)
version = infoOption (Data.Version.showVersion Meta.version)
  (  short 'v'
  <> long "version"
  <> help "Print version information" )
  

-- Commands
parseCommand :: Parser Command
parseCommand = subparser $
    command "find"          (parseFindBook      `withInfo` "Find a book") <>
    command "findAuthor"    (parseFindAuthor    `withInfo` "Find an author") <>
    command "showFollowers" (parseShowFollowers `withInfo` "Show followers of user with id") <>
    command "show"          (parseShowShelf     `withInfo` "Show a shelf, e.g. to-read") <>
    command "book"          (parseShowBook      `withInfo` "Show information about a book.") <>
    command "add"           (parseAddBook       `withInfo` "Add a book to a shelf.")

parseShowBook :: Parser Command
parseShowBook = ShowBook
    <$> argument auto (metavar "BOOK_ID_OR_TITLE")
    
parseAddBook :: Parser Command
parseAddBook = AddBook
    <$> argument str  (metavar "SHELFNAME")
    <*> argument auto (metavar "BOOK_ID")  

parseFindBook :: Parser Command
parseFindBook = FindBook
    <$> argument str (metavar "TITLE")

parseFindAuthor :: Parser Command
parseFindAuthor = FindAuthor
    <$> argument str (metavar "AUTHORNAME")

parseShowFollowers :: Parser Command
parseShowFollowers = ShowFollowers
    <$> argument auto (metavar "GOODREADS_USER_ID" <> value 0) -- FIXME arguments to commands?

parseShowShelf :: Parser Command
parseShowShelf = ShowShelf
    <$> argument str  (metavar "SHELFNAME")
    <*> argument auto (metavar "GOODREADS_USER_ID" <> value 0)  -- if default-user-id is defined in config, use it as default.

main :: IO ()
main =
    run =<<
    execParser (parseOptions `withInfo` "Interact with the Goodreads API. See --help for options.")

run :: Options -> IO ()
run (Options app cmd) =
    case cmd of
        FindBook bookTitle -> G.doFindBook app bookTitle
        FindAuthor authorName -> G.doFindAuthor app authorName
        ShowFollowers uID -> print uID
        ShowShelf shelfName uID -> G.doShowShelf app shelfName uID
        AddBook shelfName bookID -> G.doAddBook app shelfName bookID
        ShowBook bookID -> G.doShowBook app bookID
