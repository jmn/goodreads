{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Goodreads API and Client as a single file
-- TODO: Use Data.AppSettings instead of old Gr.Config
module G (getBooksFromShelf, getUserFollowers, getFindAuthorByName) where

import Control.Exception (SomeException)
import Control.Monad.Catch (MonadThrow)
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack)
import Data.ByteString.UTF8 (ByteString)
import qualified Data.ByteString.UTF8 as BSU
import Data.Foldable (for_)
import Data.List
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Version (showVersion)
import Network.HTTP (urlEncode)
import Network.HTTP.Client (Manager)
import Network.URL (exportParams)
import Web.Authenticate.OAuth

import Network.HTTP.Client -- hiding (httpLbs)
import Network.HTTP.Types.Header (HeaderName)
import Network.HTTP.Simple
       (Request, parseRequest, getResponseHeader, setRequestQueryString,
        getResponseBody)
import Text.XML.Lens (Document, Element, (^?), (./), (^..), (??), root,  el, text, lengthOf)
import Options.Applicative
import qualified Data.ByteString.Lazy.Char8 as L8
import Network.HTTP.Client (newManager, responseBody) -- hiding (httpLbs)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Text.XML (parseText_, def)
import qualified Data.CaseInsensitive as CI
import qualified Data.ByteString.Char8 as BytCh
import qualified Paths_g as Meta (version)
import Network.HTTP.Client (Manager, Request)
import Web.Authenticate.OAuth
       (oauthConsumerKey, oauthConsumerSecret, newOAuth, unCredential,
        newCredential, signOAuth)
import System.Environment (getEnv)
import Control.Monad.IO.Class (liftIO)
import Data.AppSettings (Setting(..), GetSetting(..),
  FileLocation(Path), readSettings, DefaultConfig)
  
data Gr = Gr         { config :: GrConfig
                     , connectionManager :: Manager
                     , appCredentials :: AppCredentials
                     }

data GrConfig = GrConfig { loginCredentials :: Credential
                         , defaultUserID    :: Maybe Int
                                 } deriving (Show, Eq)


type AuthorName = String
data User = User
    { uid :: Int
    , name :: Maybe Text} deriving (Show)

-- Begin Auth Stuff
data AppCredentials = AppCredentials { applicationKey :: BS.ByteString
                                     , applicationSecret :: BS.ByteString
                                     } deriving (Show, Eq)

type AuthHandler = String -> IO BS.ByteString

data AuthRequest = AuthRequest { applicationName :: String
                               , expiration :: Maybe Int
                               , scope :: [AuthScope]
                               , requestAppCredentials :: AppCredentials
                               } deriving (Show)

data AuthScope = Read
               | Write

instance Show AuthScope where
    show Read = "read"
    show Write = "write"

data Book = Book {
    title :: Text
  , pub_yr :: Text  -- FIXME: Use time/proper type
    } deriving Show

type ShelfName = String
type UserID = Int

data Options = Options AppOptions Command
data Command
     = FindAuthor AuthorName 
     | ShowFollowers UserID
     | ShowShelf ShelfName UserID

data AppOptions = AppOptions
    { apiKey :: Maybe String
    , limit  :: Maybe Int
    }
    
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
 	Right (conf, GetSetting getSetting) -> do
 		let defUser = getSetting defaultUser
                return defUser
 		-- saveSettings emptyDefaultConfig (AutoFromAppName "test") conf
                --    cfg <- liftIO $ loadConfig $ defaultConfig man req authMethod
                --    writeConfig cfg
                --     return $ Gr cfg man (requestAppCredentials req)
        Left (x :: SomeException) -> error "Error reading the config file!"


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
--  <*> t "author"

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


doShowShelf :: AppOptions -> ShelfName -> UserID -> IO ()
doShowShelf opts shelf uID = do
    gr <- doGr opts
    r <-
        signed gr $
        getBooksFromShelf
            gr
            User
            { uid = uID
            , G.name = Nothing
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
    keys <-
        case apiKey app of
            Just k ->
                return
                    AppCredentials
                    { applicationKey = pack k
                    , applicationSecret = pack "NOT IMPLEMENTED"
                    }
            Nothing -> getKeysFromEnv
    let authReq =
            AuthRequest
            { applicationName = "Gr"
            , expiration = Nothing
            , scope = [Read, Write]
            , requestAppCredentials = keys
            }
    manager <- newManager tlsManagerSettings
    initGr manager authReq defaultAuthHandler

-- defaultUser :: Maybe Int -- TODO fetch from config file
-- defaultUser = Just 

defaultUser :: Setting Int
defaultUser = Setting "defaultUser" 35682014
-- c :: Setting Credential

defaultConfig :: DefaultConfig
defaultConfig = getDefaultConfig $ do
    setting defaultUser
    
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
    command "findAuthor"    (parseFindAuthor    `withInfo` "Find an author") <>
    command "showFollowers" (parseShowFollowers `withInfo` "Show followers of user with id") <>
    command "show"          (parseShowShelf defaultUser   `withInfo` "Show a shelf, e.g. to-read")

parseFindAuthor :: Parser Command
parseFindAuthor = FindAuthor
    <$> argument str (metavar "AUTHORNAME")

parseShowFollowers :: Parser Command
parseShowFollowers = ShowFollowers
    <$> argument auto (metavar "GOODREADS_USER_ID") -- FIXME arguments to commands?


parseShowShelf :: Maybe Int -> Parser Command
parseShowShelf mDef = ShowShelf
    <$> argument str  (metavar "SHELFNAME")
    <*> argument auto (metavar "GOODREADS_USER_ID" <> maybe mempty value mDef)  -- if default-user-id is defined in config, use it as default.

main :: IO ()
main =
    run =<<
    execParser (parseOptions `withInfo` "Interact with the Goodreads API. See --help for options.")

run :: Options -> IO ()
run (Options app cmd) =
    case cmd of
        FindAuthor authorName -> doFindAuthor app authorName
        ShowFollowers uID -> print uID
        ShowShelf shelfName uID -> doShowShelf app shelfName uID
