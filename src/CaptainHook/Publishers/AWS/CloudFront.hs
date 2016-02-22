{-# LANGUAGE OverloadedStrings #-}

module CaptainHook.Publishers.AWS.CloudFront (
  DistStatusResponse(..)
, getDistStatusResponse
) where

import           Configuration.Dotenv        (loadFile)
import           Control.Lens                (view)
import           Control.Monad.IO.Class      (MonadIO (..), liftIO)
import           Data.String                 (IsString (..))
import           Data.Text                   (Text (..))
import           Data.Time.Clock             (UTCTime (..), getCurrentTime)
import           Network.AWS                 (AccessKey (..),
                                              Credentials (FromKeys), Env (..),
                                              MonadAWS (..), Region (Oregon),
                                              SecretKey (..), newEnv, runAWS,
                                              runResourceT, send)
import           Network.AWS.CloudFront      (Distribution (..), dId, dStatus,
                                              gdrsDistribution, getDistribution)
import qualified Network.AWS.Data.ByteString as AwsBS

import           CaptainHook.Config          (getEnvVar)

data DistStatus = Deployed | InProgress deriving (Eq, Show)

data DistStatusResponse = DistStatusResponse
  { distId       :: Text
  , distStatus   :: DistStatus
  , responseTime :: UTCTime
  } deriving (Eq, Show)

instance Show SecretKey where
  show (SecretKey s) = show s

data AWSConfig = AWSConfig
  { accessKey :: AccessKey
  , secretKey :: SecretKey
  , region    :: Region
  } deriving (Eq, Show)

distStatusResponse :: Distribution -> UTCTime -> DistStatusResponse
distStatusResponse dist resTime =
  DistStatusResponse {
    distId        = view dId dist
  , distStatus    = parseDistStatus dist
  , responseTime  = resTime
  }

awsConfig :: AwsBS.ByteString -> AwsBS.ByteString -> Region -> AWSConfig
awsConfig aKey sKey r =
  AWSConfig {
    accessKey = (AccessKey aKey)
  , secretKey = (SecretKey sKey)
  , region    = r
  }

parseDist :: Monad m => Maybe Distribution -> m Distribution
parseDist (Just dist) = return dist
parseDist Nothing     = fail "Couldn't parse distribution"

parseRegion :: (Monad m, IsString s, Eq s) => s -> m Region
parseRegion "Oregon" = return Oregon
parseRegion _        = fail "Unknown region"

parseDistStatus :: Distribution -> DistStatus
parseDistStatus dist =
  case status of
    "Deployed" -> Deployed
    _          -> InProgress
  where status = view dStatus dist

getAWSConfig :: IO AWSConfig
getAWSConfig = do
  accessKeyId <- getEnvVar "AWS_ACCESS_KEY_ID"
  secret      <- getEnvVar "AWS_SECRET_ACCESS_KEY"
  regionText  <- getEnvVar "AWS_REGION"
  region      <- parseRegion regionText
  return $ awsConfig accessKeyId secret region

getAWSEnv :: IO Env
getAWSEnv = do
  awsConf <- getAWSConfig
  region  <- return $ region awsConf
  creds   <- return $ FromKeys (accessKey awsConf) (secretKey awsConf)
  newEnv region creds

getDist :: MonadAWS io => Text -> io Distribution
getDist distId = do
  response  <- send $ getDistribution distId
  maybeDist <- return $ view gdrsDistribution response
  parseDist maybeDist

getDistStatusResponse :: IO DistStatusResponse
getDistStatusResponse = do
  loadFile False "./.env"
  env             <- getAWSEnv
  distributionId  <- getEnvVar "AWS_CLOUDFRONT_DISTRIBUTION"
  runResourceT . runAWS env $ do
    dist <- getDist distributionId
    now  <- liftIO getCurrentTime
    return $ distStatusResponse dist now

