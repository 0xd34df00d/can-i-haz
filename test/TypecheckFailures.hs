{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors -Wno-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TypecheckFailures where

import Control.Monad.Reader.Has

import Common

instance Has FooEnv BarEnv

extractMissing :: FooEnv -> BarEnv
extractMissing = extract

extractMultiple :: (FooEnv, FooEnv) -> FooEnv
extractMultiple = extract
