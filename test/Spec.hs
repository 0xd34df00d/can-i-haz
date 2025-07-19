{-# LANGUAGE MultiParamTypeClasses, RankNTypes, GADTs #-}
{-# LANGUAGE TypeOperators #-}

import Test.Hspec
import Test.HUnit.Lang

import Control.DeepSeq
import Control.Exception
import Data.List

import Control.Monad.Except.CoHas
import Control.Monad.Reader.Has

import Common
import TypecheckFailures

-- Reimplementing due to https://github.com/CRogers/should-not-typecheck/issues/18
shouldNotTypecheck :: NFData a => (() ~ () => a) -> Assertion
shouldNotTypecheck a = do
  result <- try (evaluate $ force a)
  case result of
    Right _ -> assertFailure "Expected expression to not compile but it did compile"
    Left e@(TypeError msg) -> if "(deferred type error)" `isSuffixOf` msg
                                 then pure ()
                                 else throwIO e

fooEnvTwice :: FooEnv -> FooEnv
fooEnvTwice (FooEnv n s) = FooEnv (n * 2) (s <> s)

barEnvCons :: BarEnv -> BarEnv
barEnvCons (BarEnv d xs) = BarEnv d (0 : xs)

main :: IO ()
main = hspec $ do
  let baseFooEnv = FooEnv 10 "meh"
  let baseFooEnvTwice = FooEnv 20 "mehmeh"
  let baseBarEnv = BarEnv 4.2 [1, 2, 3]
  let baseBarEnvCons = BarEnv 4.2 [0, 1, 2, 3]
  describe "Has" $ do
    describe "Basic instance" $ do
      it "any type Has itself" $ do
        let exFoo = extract baseFooEnv
        exFoo `shouldBe` baseFooEnv
      it "any type updates itself" $ do
        let baseFooEnv' = update fooEnvTwice baseFooEnv
        baseFooEnv' `shouldBe` baseFooEnvTwice
    describe "Generic instances" $ do
      it "envs have their components" $ do
        let exFoo = extract $ AppEnv baseFooEnv baseBarEnv
        exFoo `shouldBe` baseFooEnv
        let exBar = extract $ AppEnv baseFooEnv baseBarEnv
        exBar `shouldBe` baseBarEnv
      it "envs update their components" $ do
        let appEnv' = update fooEnvTwice $ update barEnvCons $ AppEnv baseFooEnv baseBarEnv
        appEnv' `shouldBe` AppEnv baseFooEnvTwice baseBarEnvCons
      it "tuples have their components" $ do
        let exFoo = extract (baseFooEnv, baseBarEnv)
        exFoo `shouldBe` baseFooEnv
        let exBar = extract (baseFooEnv, baseBarEnv)
        exBar `shouldBe` baseBarEnv
      it "tuples update their components" $ do
        let pair' = update fooEnvTwice $ update barEnvCons (baseFooEnv, baseBarEnv)
        pair' `shouldBe` (baseFooEnvTwice, baseBarEnvCons)
    describe "Should not typecheck" $ do
      it "if there is no such type in the hierarchy" $ shouldNotTypecheck extractMissing
      it "if there is more than one such type in the hierarchy" $ shouldNotTypecheck extractMultiple
  describe "CoHas" $ do
    describe "Basic instance" $
      it "any type CoHas itself" $ do
        let injected = 10 :: Int
        let inFoo = inject injected
        inFoo `shouldBe` injected
    describe "Generic instances" $ do
      it "sums have their components" $ do
        let inFoo = inject baseFooEnv
        inFoo `shouldBe` FooEnvErr baseFooEnv
        let inBar = inject baseBarEnv
        inBar `shouldBe` BarEnvErr baseBarEnv
      it "Either has its components" $ do
        let inFoo = inject baseFooEnv :: Either FooEnv Int
        inFoo `shouldBe` Left baseFooEnv
        let inBar = inject baseBarEnv :: Either Int BarEnv
        inBar `shouldBe` Right baseBarEnv
    describe "Should not typecheck" $ do
      it "if there is no such type in the hierarchy" $ shouldNotTypecheck injectMissing
      it "if there is more than one such type in the hierarchy" $ shouldNotTypecheck injectMultiple
