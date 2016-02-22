{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module CaptainHook.Subscribers.Slack (
  postMessage
) where

import           Configuration.Dotenv (loadFile)
import           Control.Lens         ((^.))
import           Data.Aeson           (ToJSON, toJSON)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.Wreq         (FormParam ((:=)), Response, post,
                                       responseStatus, statusCode)

import           CaptainHook.Config   (getEnvVar)

data WebhookRequest = WebhookRequest
  { text :: String } deriving (Generic, Show)

instance ToJSON WebhookRequest

webhookRequest :: String -> WebhookRequest
webhookRequest msg = WebhookRequest { text = msg }

postMessage msg = do
  let params = webhookRequest msg
  loadFile False "./.env" -- TODO do this only once
  webhookUrl <- getEnvVar "SLACK_WEBHOOK_URL"
  res        <- post webhookUrl $ toJSON params
  return $ res

