{-# LANGUAGE TypeOperators, DataKinds, PolyKinds, TypeFamilies, ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables, DefaultSignatures #-}
{-# LANGUAGE Safe #-}

module Control.Monad.Except.CoHas
( CoHas(..)
, SuccessfulSearch
) where

import Data.Proxy
import GHC.Generics

import Data.Path

type family Search part (g :: k -> *) :: MaybePath where
  Search part (K1 _ part) = 'Found 'Here
  Search part (K1 _ other) = 'NotFound
  Search part (M1 _ _ x) = Search part x
  Search part (f :+: g) = Combine (Search part f) (Search part g)
  Search _ _ = 'NotFound

class GCoHas (path :: Path) part grecord where
  ginject :: Proxy path -> part -> grecord p

instance GCoHas 'Here rec (K1 i rec) where
  ginject _ = K1

instance GCoHas path part record => GCoHas path part (M1 i t record) where
  ginject proxy = M1 . ginject  proxy

instance GCoHas path part l => GCoHas ('L path) part (l :+: r) where
  ginject _ = L1 . ginject (Proxy :: Proxy path)

instance GCoHas path part r => GCoHas ('R path) part (l :+: r) where
  ginject _ = R1 . ginject (Proxy :: Proxy path)

-- | Type alias representing that the search of @part@ in @record@ has been successful.
--
-- The @path@ is used to guide the default generic implementation of 'Has'.
type SuccessfulSearch part record path = (Search part (Rep record) ~ 'Found path, GCoHas path part (Rep record))

class CoHas part record where
  inject :: part -> record

  default inject :: forall path. (Generic record, SuccessfulSearch part record path) => part -> record
  inject = to . ginject (Proxy :: Proxy path)

instance CoHas record record where
  inject = id

instance SuccessfulSearch l (Either l r) path => CoHas l (Either l r)
instance SuccessfulSearch r (Either l r) path => CoHas r (Either l r)
