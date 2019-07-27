{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Common where

import Control.DeepSeq
import GHC.Generics

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
