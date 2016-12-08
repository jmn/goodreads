module Types where
import Data.Text (Text)
import qualified Data.ByteString as BS
import Network.HTTP.Client (Manager)
import Web.Authenticate.OAuth
       (oauthConsumerKey, oauthConsumerSecret, newOAuth, unCredential,
        newCredential, signOAuth, Credential (Credential))

data Options = Options AppOptions Command
data Command
     = FindAuthor AuthorName 
     | ShowFollowers UserID
     | ShowShelf ShelfName UserID
     | FindBook BookTitle
     | AddBook ShelfName BookID
     | ShowBook BookID --(Either BookID BookTitle)
     
type ShelfName = String
type BookTitle = String
type UserID = Int
type BookID = Int

data AppOptions = AppOptions
    { apiKey :: Maybe String
    , limit  :: Maybe Int
    }

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
  , bookId :: Maybe Text

    } deriving Show

