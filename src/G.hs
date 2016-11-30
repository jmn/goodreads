{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Goodreads API and Client as a single file
-- TODO: Use Data.AppSettings instead of old Gr.Config
module G (doShowShelf, doFindAuthor, getBooksFromShelf, getUserFollowers, getFindAuthorByName) where
import Types
import Data.Text (Text)
--import Data.Serialization
--import Control.Monad.Catch (MonadThrow)
import Control.Monad (guard)
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack)
import Data.ByteString.UTF8 (ByteString)
import qualified Data.ByteString.UTF8 as BSU
import Data.List
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Data.Text.Lazy.Encoding (decodeUtf8)
import Network.HTTP (urlEncode)
import Data.Foldable (for_)
import Web.Authenticate.OAuth
import Network.URL (exportParams)
-- import Network.HTTP.Client -- hiding (httpLbs)
import Network.HTTP.Types.Header (HeaderName)
import Network.HTTP.Simple
       (Request, Response, parseRequest, getResponseHeader, setRequestQueryString,
        getResponseBody)
import Text.XML.Lens (Document, Element, (^?), (./), (^..), (??), root,  el, text, lengthOf)
import qualified Data.ByteString.Lazy.Char8 as L8
import Network.HTTP.Client (newManager, responseBody, httpLbs) -- hiding (httpLbs)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Text.XML (parseText_, def)
import qualified Data.CaseInsensitive as CI
import TextShow (printT)
import qualified Data.ByteString.Char8 as BytCh
import Network.HTTP.Client (Manager, Request, newManager, responseBody)
import Web.Authenticate.OAuth
       (oauthConsumerKey, oauthConsumerSecret, newOAuth, unCredential,
        newCredential, signOAuth, Credential (Credential))
import System.Environment (getEnv)
import Data.AppSettings (Setting(..), GetSetting(..),
  FileLocation(Path), readSettings, DefaultConfig, FileLocation( AutoFromAppName), getDefaultConfig, setting, saveSettings, emptyDefaultConfig, setSetting, ParseException(..), Conf)
import Control.Exception.Safe -- (IOException(..), catches, try, throw, Exception)
import System.IO.Error (isDoesNotExistError)
import qualified Data.Map as Map

--import Data.Typeable (Typeable)

  -- | The configuration file is in an invalid format.
-- data ParseException = ParseException FilePath String
--     deriving (Show, Typeable)
-- instance Exception ParseException

-- Begin Auth Stuff
getKeysFromEnv :: IO AppCredentials
getKeysFromEnv = do
  grApiKey    <- getEnv "GOODREADS_API_KEY"
  grApiSecret <- getEnv "GOODREADS_API_SECRET"
  return AppCredentials { applicationKey = pack grApiKey
                        , applicationSecret = pack grApiSecret}


-- Sign a request using config
signWithConfig :: Gr -> Request -> IO Request
signWithConfig gr request = do
     let appCreds = appCredentials gr
     let tokenString = snd (head (unCredential (loginCredentials (config gr))))
     let tokenSecretString = snd (head (tail (unCredential (loginCredentials (config gr)))))
     let mycred = newCredential tokenString tokenSecretString
     let myoauth = newOAuth { oauthConsumerKey = applicationKey appCreds, oauthConsumerSecret = applicationSecret appCreds }
     signOAuth myoauth mycred request

-- removeIfExists :: FilePath -> IO ()
-- removeIfExists fileName = removeFile fileName `catch` handleExists
--  where handleExists e
--          | isDoesNotExistError e = return ()
--          | otherwise = throwIO e

-- result :: IO (Maybe String)
-- result = foo <$> getString

-- foo :: String -> Maybe String
-- foo s | pred s    = Just (f s)
--       | otherwise = Nothing

-- r2 :: IO (Maybe (Conf, GetSetting))
-- r2 = foo2 <$> readSettings (AutoFromAppName "goodreads") `catch` handleExists

-- foo2 :: (Conf, GetSetting) -> (Maybe (Conf, GetSetting))
-- foo2 s
--   | pred s = Just s
--   | otherwise = Nothing

--handleExistsz :: (RealFloat a) => a -> a -> String
handleExists e
    | isDoesNotExistError e = return Nothing
    | otherwise = throwIO e
{- Problem:
   Load Config file, if it does not exist, create it, if there's an IO error: throw it. -}

--loadConfig :: undefined -- IO (Maybe (Conf, GetSetting))

lookupConfig :: IO (Maybe (Conf, GetSetting))
lookupConfig = catchJust (guard . isDoesNotExistError) (Just <$> (readSettings (AutoFromAppName "goodreads"))) (\e -> return Nothing)

initGr :: Manager -> AuthRequest -> AuthHandler -> IO Gr
initGr man req authMethod = do
        x <- lookupConfig
        case x of
          Just (conf, GetSetting getSetting) -> do
                      let secret = getSetting oAuthSecret
                      case secret of
                          "" -> do putStrLn "No OAuth token found" -- getnewsecrets?
                                   credentials <- grAuthenticate man req authMethod
                                   let tokenString = BSU.toString $ snd (head (unCredential credentials))
                                   let tokenSecretString = BSU.toString $ snd (head (tail (unCredential credentials)))
                                   let conf1 = setSetting conf oAuthToken tokenString
                                   let conf2 = setSetting conf1 oAuthSecret tokenSecretString

                                   saveSettings emptyDefaultConfig (AutoFromAppName "goodreads") conf2
                                   putStrLn "Saved new OAauth token and secret to config file"
                                   let cfg = GrConfig {loginCredentials = newCredential (pack $ getSetting oAuthToken) (pack $ secret)
                                                      , defaultUserID = Just (getSetting defaultUser)}
                                   return $ Gr cfg man (requestAppCredentials req)
                          _ -> do
                                 let cfg = GrConfig {loginCredentials = newCredential (pack $ getSetting oAuthToken) (pack $ secret)
                                                   , defaultUserID = Just (getSetting defaultUser)}
                                 saveSettings emptyDefaultConfig (AutoFromAppName "goodreads") conf
                                 putStrLn "Loaded config file" -- ++ (AutoFromAppName "test")
                                 return $ Gr cfg man (requestAppCredentials req)
          Nothing -> do putStrLn "No config file found."
                        credentials <- grAuthenticate man req authMethod
                        let tokenString = BSU.toString $ snd (head (unCredential credentials))
                        let tokenSecretString = BSU.toString $ snd (head (tail (unCredential credentials)))
                        let conf1 = setSetting Map.empty oAuthToken tokenString
                        let conf2 = setSetting conf1 oAuthSecret tokenSecretString
                        saveSettings emptyDefaultConfig (AutoFromAppName "goodreads") conf2
                        putStrLn "Saved new OAauth token and secret to config file"
                        let cfg = GrConfig {loginCredentials = newCredential (pack $ tokenString) (pack $ tokenSecretString)
                                           , defaultUserID = Nothing}
                        return $ Gr cfg man (requestAppCredentials req)

numReviews :: Document -> Int
numReviews doc = lengthOf ?? doc $ root . el "GoodreadsResponse" ./ el "reviews" ./ el "review"

-- tities :: Document -> [DI.Text]
tities :: Document -> [Text]
tities doc = doc ^.. root . el "GoodreadsResponse" ./ el "reviews" ./ el "review" ./ el "book" ./ el "title" . text

parseGoodreadsFeed :: Document -> Either String [Book]
parseGoodreadsFeed doc =
  let bookElems = doc ^.. root . el "GoodreadsResponse" ./ el "reviews" ./ el "review"
      books = catMaybes $ fmap parseBook bookElems

  in if null bookElems
      then Left $ "Unable to parse any items from " <> show doc
      else if (length bookElems) /= (length books)
           then Left $ "Unable to parse all items from " <> show bookElems
           else Right books

parseBook :: Element -> Maybe Book
parseBook e = Book
  <$> t "title"
  <*> t "description" -- FIXME: Handle non existing fields

  where t n = e ^? el "review" ./ el "book" ./ el n . text


toHeaderName :: String -> HeaderName
toHeaderName header = CI.mk (BytCh.pack header)

respInfo :: Response L8.ByteString -> IO ()
respInfo resp = print $ getResponseHeader (toHeaderName "content-type") resp

--signed :: Gr -> IO Request -> IO (Response L8.ByteString)
signed :: Gr -> Request -> IO (Response L8.ByteString)
signed mgr resp = do
    signed_req <- signWithConfig mgr resp
    httpLbs signed_req (connectionManager mgr)

requestParameters :: AuthRequest -> [(String, String)]
requestParameters (AuthRequest name exp scopes _) =
    [ ("name", name)
    , ("expiration", expirationStr exp)
    , ("scope", scopeStr scopes) ]
  where expirationStr Nothing = "never"
        expirationStr (Just d) | d == 1 = "1day"
                               | otherwise = (show d) ++ "days"
        scopeStr scopes = intercalate "," $ map show scopes

oauthGr :: AppCredentials -> OAuth
oauthGr (AppCredentials key secret) =
    def { oauthConsumerKey    = key
        , oauthConsumerSecret = secret
        , oauthRequestUri     = "https://www.goodreads.com/oauth/request_token"
        , oauthAuthorizeUri   = "https://www.goodreads.com/oauth/authorize"
        , oauthAccessTokenUri = "https://www.goodreads.com/oauth/access_token"
        , oauthServerName     = "https://www.goodreads.com/"
        , oauthSignatureMethod = HMACSHA1} -- FIXME HTTPS

grAuthorizeUrl :: AuthRequest -> OAuth -> Credential -> String
grAuthorizeUrl req oauth cred =
    concat [ baseUrl
           , "&"
           , exportParams $ reqParams]
  where
    baseUrl = authorizeUrl oauth cred
    reqParams = requestParameters req

grAuthenticate :: Manager -> AuthRequest -> AuthHandler -> IO Credential
grAuthenticate man req handleAuth = do
    cred <- getTemporaryCredential oauthSettings man

    verifier <- handleAuth $  grAuthorizeUrl req oauthSettings cred

    getAccessToken oauthSettings
                   (injectVerifier verifier cred)
                   man
  where appCreds = requestAppCredentials $ req
        oauthSettings = oauthGr appCreds

defaultAuthHandler :: AuthHandler
defaultAuthHandler url = do
    putStrLn "You will need to authorize Gr to use your account."
    putStrLn "Point your web browser to:"
    putStrLn ""
    putStrLn url
    putStrLn ""
    putStrLn "Paste the verification code you are given below and hit enter"
    BS.getLine

--- End Auth methods

--- Begin Api Methods
restAPI :: MonadThrow m => Gr -> String -> [(ByteString, Maybe ByteString)] -> m Request
-- restAPI :: Gr -> String -> [(ByteString, Maybe ByteString)] -> Request
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

--getFindAuthorByName :: Control.Monad.Catch.MonadThrow m => Gr -> AuthorName -> m Request
getFindAuthorByName conMan authorName = do
    restAPI conMan ("api/author_url/" ++ (urlEncode authorName)) []


getUserFollowers :: MonadThrow m => Gr -> User -> m Request -- getUserFollowers :: Gr -> User -> Maybe Request
getUserFollowers conMan user =
    restAPI conMan ("user/" ++ show (uid user) ++ "/followers.xml") []

--getBooksFromShelf :: Control.Monad.Catch.MonadThrow m => Gr -> User -> String -> m Request -- getUserFollowers :: Gr -> User -> Maybe Request
getBooksFromShelf conMan user shelf =
    restAPI conMan ("review/list/" ++ show (uid user) ++ ".xml") opts where
      opts = [
            (pack "v",      Just $ pack "2")
          , (pack "shelf",  Just $ pack shelf)
          ]  :: [(ByteString, Maybe ByteString)]



-- defaultUser :: Maybe Int -- TODO fetch from config file
-- defaultUser = Just

defaultUser :: Setting Int
defaultUser = Setting "defaultUser" 35682014
-- c :: Setting Credential
oAuthToken :: Setting String
oAuthToken = Setting "oAuthSecret" ""

oAuthSecret :: Setting String
oAuthSecret = Setting "oAuthToken" ""

defaultConfig :: DefaultConfig
defaultConfig = getDefaultConfig $ do
    setting defaultUser


doShowShelf :: AppOptions -> ShelfName -> UserID -> IO ()
doShowShelf opts shelf 0 = putStrLn "Please provide a valid User ID."
doShowShelf opts shelf uID = do
    gr <- doGr opts
    req <- getBooksFromShelf gr
            User
            { uid = uID
            , name = Nothing  }
            shelf

    resp <-  signed gr req -- try
    let eBooks = respToBooks resp
    case eBooks of
        Right books -> do for_ books $ \book -> printT $ title book
                          print req --resp --- FIXME: CASE DEBUG?
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
            Nothing -> getKeysFromEnv -- FIXME Get from config file?
    case keys of
       Left (x :: SomeException) -> error "Error Loading API Keys: Set GOODREADS_API_KEY, GOODREADS_API_SECRET"
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
