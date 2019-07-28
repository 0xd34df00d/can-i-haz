{-# LANGUAGE TypeOperators, DataKinds, PolyKinds, TypeFamilies, ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables, DefaultSignatures #-}
{-# LANGUAGE Safe #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Description : Generic implementation of the Has pattern
Stability   : experimental

This module defines a class 'Has' intended to be used with the 'Control.Monad.Reader.MonadReader' class
or 'Control.Monad.Reader.Reader' / 'Control.Monad.Reader.ReaderT' types.

= The problem

Assume there are two types representing the 'Control.Monad.Reader.MonadReader' environments
for different parts of an app:

@
data DbConfig = DbConfig { .. }
data WebConfig = WebConfig { .. }
@

as well as a single type containing both of those:

@
data AppEnv = AppEnv
  { dbConfig :: DbConfig
  , webConfig :: WebConfig
  }
@

What should be the @MonadReader@ constraint of the DB module and web module respectively?

1. It could be @MonadReader AppEnv m@ for both, introducing unnecessary coupling.

2. Or it could be @MonadReader DbConfig m@ for the DB module and
   @MonadReader WebConfig m@ for the web module respectively, but combining them becomes a pain.

Or, it could be @MonadReader r m, Has DbConfig r@ for the DB module (and similarly for the web module),
where some appropriately defined @Has part record@ class allows projecting @part@ out of some @record@.
This approach keeps both modules decoupled, while allowing using them in the same monad stack.

The only downside is that now one has to define the @Has@ class and write tediuos instances for the @AppEnv@ type
(and potentially other types in case of tests).

But why bother doing the work that the machine will happily do for you?

= The solution

This module defines the generic 'Has' class as well as hides all the boilerplate behind "GHC.Generics",
so all you have to do is to add the corresponding @deriving@-clause:

@
data AppEnv = AppEnv
  { dbConfig :: DbConfig
  , webConfig :: WebConfig
  } deriving (Generic, Has DbConfig, Has WebConfig)
@

and use @ask extract@ instead of @ask@ (but this is something you'd have to do anyway).

-}

module Control.Monad.Reader.Has
( Has(..)
, SuccessfulSearch
) where

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

-- | Type alias representing that the search of @part@ in @record@ has been successful.
--
-- The @path@ is used to guide the default generic implementation of 'Has'.
type SuccessfulSearch part record path = (Search part (Rep record) ~ 'Found path, GHas path part (Rep record))

-- | The @Has part record@ class is used for records of type @record@ supporting
-- projecting out a value of type @part@.
class Has part record where
  -- | Extract a subvalue of type @part@ from the @record@.
  --
  -- The default implementation searches for some value of the type @part@ in @record@ (potentially recursively)
  -- and returns that value. The default implementation typechecks if and only if
  -- there is a single subvalue of type @part@ in @record@.
  extract :: record -> part

  default extract :: forall path. (Generic record, SuccessfulSearch part record path) => record -> part
  extract = gextract (Proxy :: Proxy path) . from

-- | Each type allows projecting itself (and that is an 'id' projection).
instance Has record record where
  extract = id

instance SuccessfulSearch a0 (a0, a1) path => Has a0 (a0, a1)
instance SuccessfulSearch a1 (a0, a1) path => Has a1 (a0, a1)

instance SuccessfulSearch a0 (a0, a1, a2) path => Has a0 (a0, a1, a2)
instance SuccessfulSearch a1 (a0, a1, a2) path => Has a1 (a0, a1, a2)
instance SuccessfulSearch a2 (a0, a1, a2) path => Has a2 (a0, a1, a2)
