import Test.Hspec

import Control.Monad.Reader.Has

data FooEnv = FooEnv
  { fooInt :: Int
  , fooStr :: String
  } deriving (Eq, Ord, Show)

main :: IO ()
main = hspec $ do
  let baseFooEnv = FooEnv { fooInt = 10, fooStr = "meh" }
  describe "Basic Has instance" $
    it "Any type Has itself" $ do
      let theFoo = extract baseFooEnv
      theFoo `shouldBe` baseFooEnv
