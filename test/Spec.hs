import Test.Hspec
import Test.ShouldNotTypecheck

import Control.Monad.Reader.Has

import Common
import TypecheckFailures

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
  describe "Should not typecheck" $ do
    it "if there is no such type in the hierarchy" $ shouldNotTypecheck extractMissing
    it "if there is more than one such type in the hierarchy" $ shouldNotTypecheck extractMultiple
