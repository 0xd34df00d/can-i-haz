{-# LANGUAGE TypeOperators, DataKinds, PolyKinds, TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables, DefaultSignatures #-}

module Control.Monad.Reader.Has where

import Data.Proxy
import GHC.Generics

data Path = L Path | R Path | Here deriving (Show)
data MaybePath = NotFound | Conflict | Found Path deriving (Show)

type family Combine p1 p2 where
  Combine ('Found path) 'NotFound = 'Found ('L path)
  Combine 'NotFound ('Found path) = 'Found ('R path)
  Combine 'NotFound 'NotFound = 'NotFound
  Combine _ _ = 'Conflict

type family Search part (g :: k -> *) :: MaybePath where
  Search part (K1 _ part) = 'Found 'Here
  Search part (K1 _ other) = 'NotFound
  Search part (M1 _ _ x) = Search part x
  Search part (f :*: g) = Combine (Search part f) (Search part g)
  Search _ _ = 'NotFound

class GHas (path :: Path) part grecord where
  gextract :: Proxy path -> grecord p -> part

instance GHas 'Here rec (K1 i rec) where
  gextract _ (K1 x) = x

instance GHas path part struct => GHas path part (M1 i t struct) where
  gextract proxy (M1 x) = gextract proxy x

instance GHas path part l => GHas ('L path) part (l :*: r) where
  gextract _ (l :*: _) = gextract (Proxy :: Proxy path) l

instance GHas path part r => GHas ('R path) part (l :*: r) where
  gextract _ (_ :*: r) = gextract (Proxy :: Proxy path) r

class Has part record where
  extract :: record -> part
  default extract :: forall p. (Generic record, Search part (Rep record) ~ 'Found p, GHas p part (Rep record)) => record -> part
  extract = gextract (Proxy :: Proxy p) . from

instance Has record record where
  extract = id
