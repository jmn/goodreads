-- | Stuff that might be worth keeping around for later use.

toHeaderName :: String -> HeaderName
toHeaderName header = CI.mk (BytCh.pack header)

respInfo :: Response L8.ByteString -> IO ()
respInfo resp = print $ getResponseHeader (toHeaderName "content-type") resp