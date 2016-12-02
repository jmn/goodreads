module Auth where
import Types
import Network.HTTP.Client (Manager, Response, Request, httpLbs)
import Network.URL (exportParams)
import Web.Authenticate.OAuth
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.UTF8 as BSU
import Data.List (intercalate)

oauthGr :: AppCredentials -> OAuth
oauthGr (AppCredentials key secret) =
    def { oauthConsumerKey    = key
        , oauthConsumerSecret = secret
        , oauthRequestUri     = "https://www.goodreads.com/oauth/request_token"
        , oauthAuthorizeUri   = "https://www.goodreads.com/oauth/authorize"
        , oauthAccessTokenUri = "https://www.goodreads.com/oauth/access_token"
        , oauthServerName     = "https://www.goodreads.com/"
        , oauthSignatureMethod = HMACSHA1} -- FIXME HTTPS

requestParameters :: AuthRequest -> [(String, String)]
requestParameters (AuthRequest name exp scopes _) =
    [ ("name", name)
    , ("expiration", expirationStr exp)
    , ("scope", scopeStr scopes) ]
  where expirationStr Nothing = "never"
        expirationStr (Just d) | d == 1 = "1day"
                               | otherwise = (show d) ++ "days"
        scopeStr scopes = intercalate "," $ map show scopes

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

signed :: Gr -> Request -> IO (Response L8.ByteString)
signed mgr resp = do
    signed_req <- signWithConfig mgr resp
    httpLbs signed_req (connectionManager mgr)

-- | Sign a request using config
signWithConfig :: Gr -> Request -> IO Request
signWithConfig gr request = do
     let appCreds = appCredentials gr
     let tokenString = snd (head (unCredential (loginCredentials (config gr))))
     let tokenSecretString = snd (head (tail (unCredential (loginCredentials (config gr)))))
     let mycred = newCredential tokenString tokenSecretString
     let myoauth = newOAuth { oauthConsumerKey = applicationKey appCreds, oauthConsumerSecret = applicationSecret appCreds }
     signOAuth myoauth mycred request

credz :: Credential -> (String, String)
credz c = (BSU.toString $ snd (head (unCredential c)), BSU.toString $ snd (head (tail (unCredential c))))
