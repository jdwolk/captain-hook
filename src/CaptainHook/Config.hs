{-# LANGUAGE OverloadedStrings #-}

module CaptainHook.Config (
  getEnvVar
) where

import           Data.String        (IsString (..))
import           System.Environment (getEnv)

getEnvVar :: IsString s => String -> IO s
getEnvVar varName = fmap fromString $ getEnv varName

