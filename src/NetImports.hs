module NetImports where
    
import Network.HTTP.Simple
       (Request, parseRequest,
        setRequestQueryString, getResponseBody)
import Data.ByteString.UTF8 (ByteString)
import Network.HTTP.Client (newManager, responseBody, Manager, httpLbs, method)
