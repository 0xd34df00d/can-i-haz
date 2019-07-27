{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

import Test.Hspec

import GHC.Generics

import Control.Monad.Reader.Has

data FooEnv = FooEnv
  { fooInt :: Int
  , fooStr :: String
  } deriving (Eq, Ord, Show)

data BarEnv = BarEnv
  { barDouble :: Double
  , barArr :: [Int]
  } deriving (Eq, Ord, Show)

data AppEnv = AppEnv
  { fooEnv :: FooEnv
  , barEnv :: BarEnv
  } deriving (Eq, Ord, Show, Generic, Has FooEnv, Has BarEnv)

main :: IO ()
main = hspec $ do
  let baseFooEnv = FooEnv 10 "meh"
  let baseBarEnv = BarEnv 4.2 [1, 2, 3]
  describe "Basic Has instance" $
    it "Any type Has itself" $ do
      let exFoo = extract baseFooEnv
      exFoo `shouldBe` baseFooEnv
  describe "Generic Has instances" $ do
    it "Envs have their components" $ do
      let exFoo = extract $ AppEnv baseFooEnv baseBarEnv
      exFoo `shouldBe` baseFooEnv
      let exBar = extract $ AppEnv baseFooEnv baseBarEnv
      exBar `shouldBe` baseBarEnv
    it "Tuples have their components" $ do
      let exFoo = extract (baseFooEnv, baseBarEnv)
      exFoo `shouldBe` baseFooEnv
      let exBar = extract (baseFooEnv, baseBarEnv)
      exBar `shouldBe` baseBarEnv
