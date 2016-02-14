module CaptainHook.ServiceSpec where

import Test.Hspec
import CaptainHook.Service

spec :: Spec
spec = do
  it "is the answer to life, the universe, and everything" $ do
    theThing `shouldBe` 42
