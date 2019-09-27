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

type family Search option (g :: k -> *) :: MaybePath where
  Search option (K1 _ option) = 'Found 'Here
  Search option (K1 _ other) = 'NotFound
  Search option (M1 _ _ x) = Search option x
  Search option (f :+: g) = Combine (Search option f) (Search option g)
  Search _ _ = 'NotFound

class GCoHas (path :: Path) option gsum where
  ginject :: Proxy path -> option -> gsum p

instance GCoHas 'Here rec (K1 i rec) where
  ginject _ = K1

instance GCoHas path option sum => GCoHas path option (M1 i t sum) where
  ginject proxy = M1 . ginject  proxy

instance GCoHas path option l => GCoHas ('L path) option (l :+: r) where
  ginject _ = L1 . ginject (Proxy :: Proxy path)

instance GCoHas path option r => GCoHas ('R path) option (l :+: r) where
  ginject _ = R1 . ginject (Proxy :: Proxy path)

-- | Type alias representing that the search of @option@ in @sum@ has been successful.
--
-- The @path@ is used to guide the default generic implementation of 'Has'.
type SuccessfulSearch option sum path = (Search option (Rep sum) ~ 'Found path, GCoHas path option (Rep sum))

class CoHas option sum where
  inject :: option -> sum

  default inject :: forall path. (Generic sum, SuccessfulSearch option sum path) => option -> sum
  inject = to . ginject (Proxy :: Proxy path)

instance CoHas sum sum where
  inject = id

instance SuccessfulSearch l (Either l r) path => CoHas l (Either l r)
instance SuccessfulSearch r (Either l r) path => CoHas r (Either l r)
