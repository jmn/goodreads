{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Goodreads API and Client as a single file
-- TODO: Use Data.AppSettings instead of old Gr.Config
module GRApi (doShowShelf, doFindAuthor, doFindBook, doAddBook, doShowBook, getBooksFromShelf, getUserFollowers, getFindAuthorByName, getShowBook) where
import Types
import XML
import Auth
import System.Console.Haskeline
import Control.Monad (guard)
import Data.ByteString.Char8 (pack)
import Data.ByteString.UTF8 (ByteString)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Lazy (fromStrict)

import Formatting
import Network.HTTP (urlEncode)
import Data.Foldable (for_)
import Network.HTTP.Types.Header (HeaderName)
import Network.HTTP.Simple
       (Request, Response, parseRequest, getResponseHeader,
        setRequestQueryString, getResponseBody)
import qualified Data.ByteString.Lazy.Char8 as L8
import Network.HTTP.Client (newManager, responseBody, Manager, httpLbs, method)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Text.XML (parseText_, def)
import qualified Data.CaseInsensitive as CI
import qualified Data.ByteString.Char8 as BytCh
import Web.Authenticate.OAuth (unCredential, newCredential)
import System.Environment (getEnv)
import Data.AppSettings
       (Setting(..), GetSetting(..), readSettings, DefaultConfig,
        FileLocation(AutoFromAppName), getDefaultConfig, setting,
        saveSettings, setSetting, Conf)
import Control.Exception.Safe -- (IOException(..), catches, try, throw, Exception)
import System.IO.Error (isDoesNotExistError)
import qualified Text.Pandoc as Pandoc
-- Begin Auth Stuff
getKeysFromEnv :: IO AppCredentials
getKeysFromEnv = do
  grApiKey    <- getEnv "GOODREADS_API_KEY"
  grApiSecret <- getEnv "GOODREADS_API_SECRET"
  return AppCredentials { applicationKey = pack grApiKey
                        , applicationSecret = pack grApiSecret}

lookupConfig :: IO (Maybe (Conf, GetSetting))
lookupConfig = catchJust (guard . isDoesNotExistError) (Just <$> (readSettings (AutoFromAppName "goodreads"))) (\_ -> return Nothing)

saveConfig :: GrConfig -> AppCredentials -> IO ()
saveConfig cfg appCr = do -- conf? as 2nd arg
    let (tokenString, tokenSecretString) = credz (loginCredentials cfg)
    let defaultUID = fromMaybe 0 (defaultUserID cfg)
    let conf1 = setSetting Map.empty oAuthToken tokenString
    let conf2 = setSetting conf1 oAuthSecret tokenSecretString
    let conf3 = setSetting conf2 defaultUser defaultUID
    let conf4 = setSetting conf3 setApiKey "" -- appCredentials
    let conf5 = setSetting conf4 setApiSecret ""
    saveSettings defaultConfig (AutoFromAppName "goodreads") conf5

initGr :: Manager -> AuthRequest -> AuthHandler -> IO Gr
initGr man req authMethod = do
        x <- lookupConfig
        case x of
          Just (conf, GetSetting getSetting) -> do
                      let secret = getSetting oAuthSecret
                      case secret of
                          "" -> do putStrLn "No OAuth token found" -- getnewsecrets?
                                   credentials <- grAuthenticate man req authMethod
                                   let (tokenString, tokenSecretString) = credz credentials
                                   
                                   putStrLn "Saved new OAauth token and secret to config file"
                                   let cfg = GrConfig {loginCredentials = newCredential (pack $ tokenString) (pack $ tokenSecretString)
                                                      , defaultUserID = Just (getSetting defaultUser)}

                                   let credentials = (requestAppCredentials req)
                                   saveConfig cfg credentials
                                   return $ Gr cfg man credentials
                          _ -> do
                                 let cfg = GrConfig {loginCredentials = newCredential (pack $ getSetting oAuthToken) (pack $ secret)
                                                   , defaultUserID = Just (getSetting defaultUser)}

                                 saveSettings defaultConfig (AutoFromAppName "goodreads") conf
                                 putStrLn "Loaded config file"
                                 return $ Gr cfg man (requestAppCredentials req)
                                 
          Nothing -> do putStrLn "No config file found."
                        credentials <- grAuthenticate man req authMethod
                        let (tokenString, tokenSecretString) = credz credentials

                        putStrLn "Saved new OAauth token and secret to config file"

                        -- FIXME: Ask for a default User ID
                        -- "Would you like to add a default Goodreads user id? if so, type it and press enter."

                        let cfg = GrConfig {loginCredentials = newCredential (pack $ tokenString) (pack $ tokenSecretString)
                                           , defaultUserID = Nothing}
                        let appCreds = (requestAppCredentials req)
                        saveConfig cfg appCreds
                        return $ Gr cfg man appCreds


toHeaderName :: String -> HeaderName
toHeaderName header = CI.mk (BytCh.pack header)

respInfo :: Response L8.ByteString -> IO ()
respInfo resp = print $ getResponseHeader (toHeaderName "content-type") resp

--- Begin Api Methods
restAPI :: MonadThrow m => Gr -> String -> [(ByteString, Maybe ByteString)] -> m Request
restAPI gr endpoint params = do
    -- Add API Key to params (if it is not in there FIXME?)
    let key = (BSU.toString (applicationKey (appCredentials gr)))
    let paramsWithKey = Map.toList
                      $ Map.insert (pack "key") (Just (pack key))
                      $ Map.fromList params

    req' <- parseRequest $ "https://www.goodreads.com/" ++ endpoint
    let request
          = setRequestQueryString paramsWithKey
          $ req'
    return request

-- | Add a book to a shelf
-- https://www.goodreads.com/api/index#shelves.add_to_shelf
putAddBook :: MonadThrow m => Gr -> ShelfName -> BookID -> m Request
putAddBook conMan shelfName bookID = do
    let i = restAPI conMan ("shelf/add_to_shelf.xml") opts where
          opts = [(pack "name", Just $ pack shelfName)
            , (pack "book_id", Just $ pack $ show bookID)] :: [(ByteString, Maybe ByteString)]
    z <- i
    let req = z
            { method = "POST"
            }
    return req
          

getShowBook :: MonadThrow m => Gr -> BookID -> m Request
getShowBook conMan eBookQ = do
    -- let (uri, opts) = case eBookQ of
    --       Left bID -> ("book/show/" ++ show bID ++ ".xml", [])
    --       Right bName -> ("book/title.xml", o) where
    --                        o = [(pack "title", Just $ pack bName)] :: [(ByteString, Maybe ByteString)]

    let (uri, opts) = ("book/show/" ++ show eBookQ ++ ".xml", [])      
    restAPI conMan uri opts
    
getFindAuthorByName :: MonadThrow m => Gr -> AuthorName -> m Request
getFindAuthorByName conMan authorName = do
    restAPI conMan ("api/author_url/" ++ (urlEncode authorName)) []


getUserFollowers :: MonadThrow m => Gr -> User -> m Request 
getUserFollowers conMan user =
    restAPI conMan ("user/" ++ show (uid user) ++ "/followers.xml") []

getFindBooks :: MonadThrow m => Gr -> String -> m Request
getFindBooks conMan title =
    restAPI conMan ("search/index.xml") opts where
        opts = [
            (pack "q", Just $ pack title)] :: [(ByteString, Maybe ByteString)]
                                           
getBooksFromShelf :: MonadThrow m => Gr -> User -> String -> m Request
getBooksFromShelf conMan user shelf =
    restAPI conMan ("review/list/" ++ show (uid user) ++ ".xml") opts where
      opts = [
            (pack "v",      Just $ pack "2")
          , (pack "shelf",  Just $ pack shelf)
          ]  :: [(ByteString, Maybe ByteString)]

-- Settings
defaultUser :: Setting Int
defaultUser = Setting "defaultUser" 35682014

oAuthToken :: Setting String
oAuthToken = Setting "oAuthSecret" ""

oAuthSecret :: Setting String
oAuthSecret = Setting "oAuthToken" ""

setApiKey :: Setting String
setApiKey = Setting "apiKey" ""

setApiSecret :: Setting String
setApiSecret = Setting "apiSecret" ""

defaultConfig :: DefaultConfig
defaultConfig = getDefaultConfig $ do
    setting defaultUser

out :: T.Text -> IO ()
out txt = runInputT defaultSettings loop
   where 
       loop :: InputT IO ()
       loop = do
           outputStr $ T.unpack txt
           return ()

doFindBook :: AppOptions -> String -> IO ()
doFindBook opts findTitle = do
    gr <- doGr opts
    req <- getFindBooks gr findTitle
    resp <- httpLbs req (connectionManager gr)

    let eBooks = respToBooks resp
    case eBooks of
        Right books -> printListOfBooks books
        Left _ -> do print req                 -- FIXME: Case debug
                     fail "failed in parsing." -- FIXME: undefined -- some error in parsing See Throw, control.exceptions

    where
      respToBooks = parseBookSearch . parseText_ def . decodeUtf8 . responseBody

printListOfBooks :: [Book] -> IO ()
printListOfBooks books = do
    let booksEnumerated = (zip [1..] books)
    for_ booksEnumerated $ \(i, book) -> do 
        let msg = sformat (int % ": " % text % " [" % text % "]\n") i (fromStrict (title book)) (fromStrict $ fromMaybe "" (bookId book))
        out msg

doShowShelf :: AppOptions -> ShelfName -> UserID -> IO ()
--doShowShelf _ _ 0 = putStrLn "Please provide a valid User ID."
doShowShelf opts shelf uID = do
    gr <- doGr opts
    let user_id = case uID of
          0 -> case defaultUserID (config gr) of
                 Just u -> u
                 Nothing -> uID -- try the default user
          _ -> uID
    req <- getBooksFromShelf gr
            User
            { uid = user_id -- fixme: this shouldn't ever be called with 0
            , name = Nothing  }
            shelf

    resp <-  signed gr req -- try
    let eBooks = respToBooks resp
    case eBooks of
        Right books -> do printListOfBooks books -- for_ books $ \book -> printT $ title book
--                          print req --resp --- FIXME: CASE DEBUG?
                          putStrLn ("OAuth Used: " ++ statusOauth) where
                            statusOauth = case  (snd (head (unCredential (loginCredentials (config gr))))) of
                              "" -> "NO"
                              _  -> "YES/MAYBE"

        Left _ -> do print req -- FIXME: Case debug
                     fail "failed in parsing." -- FIXME: undefined -- some error in parsing See Throw, control.exceptions

    where
      respToBooks = parseGoodreadsFeed . parseText_ def . decodeUtf8 . responseBody

doFindAuthor :: AppOptions -> AuthorName -> IO ()
doFindAuthor opts authorName = do
    gr <- doGr opts
    req <- getFindAuthorByName gr authorName
    response <- signed gr req
    L8.putStrLn $ getResponseBody response

doAddBook :: AppOptions -> ShelfName -> BookID -> IO ()
doAddBook opts shelfName bookID = do
    gr <- doGr opts
    req <- putAddBook gr shelfName bookID
    response <- signed gr req
    L8.putStrLn $ getResponseBody response


--doShowBook :: AppOptions -> (Either BookID BookTitle) -> IO ()
doShowBook :: AppOptions -> BookID -> IO ()
doShowBook opts eBookQ = do
    gr <- doGr opts
    req <- getShowBook gr eBookQ
    response <- signed gr req
    let bookInf = parseBookInfo . parseText_ def . decodeUtf8 . responseBody

    let bookInfo = bookInf response

    L8.putStrLn $ getResponseBody response
    case bookInfo of
      Just t -> do
          let pd = Pandoc.readHtml Pandoc.def (T.unpack t)
          case pd of
            Left e -> fail "foo" --e
            Right doc -> out $ T.pack (Pandoc.writeMarkdown Pandoc.def doc)
      _ -> fail "failed"

--    L8.putStrLn $ getResponseBody response

doGr :: AppOptions -> IO Gr
doGr app = do
    keys <- try $
        case apiKey app of
            Just k -> -- Key was provided as argument
                return
                    AppCredentials
                    { applicationKey = pack k
                    , applicationSecret = pack "NOT IMPLEMENTED"
                    }
            Nothing -> do
                        x <- lookupConfig
                        case x of
                          Just (conf, GetSetting getSetting) -> do
                              let apiKey = getSetting setApiKey
                              let apiSecret = getSetting setApiSecret
                              case (any null [apiKey,apiSecret]) of
                                False -> return AppCredentials { applicationKey = pack apiKey
                                                           , applicationSecret = pack apiSecret}

                                True -> getKeysFromEnv -- not found in args, nor in config file.
                          Nothing -> getKeysFromEnv

    case keys of
       Left (_ :: SomeException) -> error "Error Loading API Keys: Set GOODREADS_API_KEY, GOODREADS_API_SECRET"
       Right k -> do
           let authReq =
                 AuthRequest
                 { applicationName = "Gr"
                 , expiration = Nothing
                 , scope = [Read, Write]
                 , requestAppCredentials = k
                 }
           manager <- newManager tlsManagerSettings
           initGr manager authReq defaultAuthHandler