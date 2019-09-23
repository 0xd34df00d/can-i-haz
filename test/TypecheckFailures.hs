{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors -Wno-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TypecheckFailures where

import Control.Monad.Except.CoHas
import Control.Monad.Reader.Has

import Common

instance Has BarEnv FooEnv

extractMissing :: FooEnv -> BarEnv
extractMissing = extract

extractMultiple :: (FooEnv, FooEnv) -> FooEnv
extractMultiple = extract

instance CoHas FooEnv BarEnv

injectMissing :: FooEnv -> BarEnv
injectMissing = inject

injectMultiple :: FooEnv -> Either FooEnv FooEnv
injectMultiple = inject
