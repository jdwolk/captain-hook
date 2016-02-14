module CaptainHook.TypesSpec where

import CaptainHook.Types (
    Publisher(..)
  , PingMethod(GET, POST)
  , makePublisher
  )
import Test.Hspec
import Data.Maybe (fromJust)
import Network.URI (parseURI)

spec :: Spec
spec = do
  describe "makePublisher" $ do
    context "given a valid endpoint URL" $ do
      it "returns a Publisher" $ do
        let urlStr = "http://www.example.com"
        let url = fromJust $ parseURI urlStr
        (fromJust $ makePublisher GET urlStr) `shouldBe` (Publisher GET url)

    context "given a non-valid endpoint URL" $ do
      it "returns Nothing" $ do
        let badUrlStr = "hotdogs"
        makePublisher GET badUrlStr `shouldBe` Nothing
