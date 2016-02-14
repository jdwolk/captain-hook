module CaptainHook.Types (
  Publisher(..),
  PingMethod(..),
  makePublisher,
) where

import Network.URI (parseURI, URI(..))

data PingMethod = GET | POST deriving (Eq, Show)

data Publisher = Publisher
  { method        :: PingMethod
  , url           :: URI
  } deriving (Eq, Show)

makePublisher :: PingMethod -> String -> Maybe Publisher
makePublisher method urlStr = do
  fmap (\ u -> Publisher { url = u, method = method }) maybeURL
    where maybeURL = parseURI urlStr
