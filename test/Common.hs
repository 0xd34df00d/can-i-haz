{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE Strict #-}

module Common where

import Control.DeepSeq
import GHC.Generics

import Control.Monad.Except.CoHas
import Control.Monad.Reader.Has

data FooEnv = FooEnv
  { fooInt :: Int
  , fooStr :: String
  } deriving (Eq, Ord, Show, Generic, NFData)

data BarEnv = BarEnv
  { barDouble :: Double
  , barArr :: [Int]
  } deriving (Eq, Ord, Show, Generic, NFData)

data AppEnv = AppEnv
  { fooEnv :: FooEnv
  , barEnv :: BarEnv
  } deriving (Eq, Ord, Show, Generic, NFData, Has FooEnv, Has BarEnv)

data AppErr = FooEnvErr FooEnv
            | BarEnvErr BarEnv
            deriving (Eq, Ord, Show, Generic, NFData, CoHas FooEnv, CoHas BarEnv)
