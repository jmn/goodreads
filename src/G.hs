{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Goodreads API and Client as a single file
-- TODO: Use Data.AppSettings instead of old Gr.Config
module G (doShowShelf, doFindAuthor, getBooksFromShelf, getUserFollowers, getFindAuthorByName) where
import Types
import Control.Exception (SomeException, try)
import Control.Monad.Catch (MonadThrow)
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack)
import Data.ByteString.UTF8 (ByteString)
import qualified Data.ByteString.UTF8 as BSU
import Data.List
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Data.Text (Text)
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
import qualified Data.ByteString.Char8 as BytCh
import Network.HTTP.Client (Manager, Request, newManager, responseBody)
import Web.Authenticate.OAuth
       (oauthConsumerKey, oauthConsumerSecret, newOAuth, unCredential,
        newCredential, signOAuth, Credential (Credential))
import System.Environment (getEnv)
import Control.Monad.IO.Class (liftIO)
import Data.AppSettings (Setting(..), GetSetting(..),
  FileLocation(Path), readSettings, DefaultConfig, FileLocation( AutoFromAppName), getDefaultConfig, setting, saveSettings, emptyDefaultConfig)
  
-- Begin Auth Stuff
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
    

-- defaultConfig :: Manager -> AuthRequest -> AuthHandler -> IO GrConfig
-- defaultConfig man req authMethod = do
--     credentials <- grAuthenticate man req authMethod
--     return $ GrConfig credentials Nothing

-- initGr :: Manager -> AuthRequest -> AuthHandler -> IO Gr
-- initGr man req authMethod = do
--     cfg <- liftIO $ loadConfig $ defaultConfig man req authMethod
--     writeConfig cfg
--     return $ Gr cfg man (requestAppCredentials req)

                                
initGr :: Manager -> AuthRequest -> AuthHandler -> IO Gr
initGr man req authMethod = do
     readResult <- try $ readSettings (AutoFromAppName "test")
     case readResult of
       Right (conf, GetSetting getSetting) -> do -- if there is a setting file it should always contain oauth credentials. (?)
                let cfg = GrConfig {loginCredentials = newCredential (pack $ getSetting oAuthToken) (pack $ getSetting oAuthSecret)
                                  , defaultUserID = Just (getSetting defaultUser)}
                saveSettings emptyDefaultConfig (AutoFromAppName "test") conf                          
                return $ Gr cfg man (requestAppCredentials req)

                -- Load OauthToken and Secret from Config, if
                -- if there is a token/secret in config, use it, otherwise go authorize

       Left (x :: SomeException) -> do
           credentials <- grAuthenticate man req authMethod
           let defConf = GrConfig credentials Nothing
           -- saveSettings emptyDefaultConfig (AutoFromAppName "test") conf                          
           return $ Gr defConf man (requestAppCredentials req)

-- error "Error reading the config file!"


numReviews doc = lengthOf ?? doc $ root . el "GoodreadsResponse" ./ el "reviews" ./ el "review"


-- tities :: Document -> [DI.Text]
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

signed :: Gr -> IO Request -> IO (Response L8.ByteString)
signed mgr resp = do
    r <- resp
    signed_req <- signWithConfig mgr r
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
restAPI :: Control.Monad.Catch.MonadThrow m => Gr -> String -> [(ByteString, Maybe ByteString)] -> m Request
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

getFindAuthorByName :: Control.Monad.Catch.MonadThrow m => Gr -> AuthorName -> m Request
getFindAuthorByName conMan authorName = do
    restAPI conMan ("api/author_url/" ++ (urlEncode authorName)) []
    

getUserFollowers :: Control.Monad.Catch.MonadThrow m => Gr -> User -> m Request -- getUserFollowers :: Gr -> User -> Maybe Request
getUserFollowers conMan user =
    restAPI conMan ("user/" ++ show (uid user) ++ "/followers.xml") []

getBooksFromShelf :: Control.Monad.Catch.MonadThrow m => Gr -> User -> String -> m Request -- getUserFollowers :: Gr -> User -> Maybe Request
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
doShowShelf opts shelf uID = do
    gr <- doGr opts
    r <-
        signed gr $
        getBooksFromShelf
            gr
            User
            { uid = uID
            , name = Nothing
            }
            shelf
    let eBooks = respToBooks r
    case eBooks of
        Right books -> for_ books $ \book -> print $ title book
        Left _ -> fail "failed in parsing." -- FIXME: undefined -- some error in parsing See Throw, control.exceptions
  where
    respToBooks = parseGoodreadsFeed . parseText_ def . decodeUtf8 . responseBody

doFindAuthor :: AppOptions -> AuthorName -> IO ()
doFindAuthor opts authorName = do
    gr <- doGr opts
    response <- signed gr $ getFindAuthorByName gr authorName
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
            Nothing -> getKeysFromEnv
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
