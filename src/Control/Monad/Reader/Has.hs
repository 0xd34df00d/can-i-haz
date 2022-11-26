{-# LANGUAGE TypeOperators, DataKinds, PolyKinds, TypeFamilies, ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables, DefaultSignatures #-}
{-# LANGUAGE Safe #-}

{-|
Description : Generic implementation of the Has pattern
Stability   : experimental

This module defines a class 'Has' intended to be used with the 'MonadReader' class
or 'Reader' / 'ReaderT' types.

= The problem

Assume there are two types representing the 'MonadReader' environments
for different parts of an application:

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
(and potentially other types in case of, for example, tests).

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

= Type safety

What should happen if @record@ does not have any field of type @part@ at all?
Of course, this means that we cannot project @part@ out of @record@, and no 'Has' instance can be derived at all.
Indeed, this library will refuse to generate an instance in this case.

On the other hand, what should happen if @record@ contains multiple values of type @part@,
perhaps on different levels of nesting? While technically we could make an arbitrary choice, like taking
the first one in breadth-first or depth-first order, we instead decide that such a choice is inherently ambiguous,
so this library will refuse to generate an instance in this case as well.

= Updating the records, or poor man's lenses, and State

One we know that a value of type @part@ is contained in @record@,
we might easily update a @record@ having a function that updates the @part@.
This is done in the obvious way: we just locate the @part@ in the @record@ and 'update' it!

'Has' has a method for this, called (unsurprisingly) 'update'.

Note that this might be used for more composable functions living in 'Control.Monad.State':
now instead of @MonadState StateType m@ we write @(MonadState s m, Has StateType s)@
and use 'update' and 'extract' where necessary
(likely in combination with 'Control.Monad.State.modify' and 'Control.Monad.State.gets').

= Exports

This module also reexports 'Control.Monad.Reader' along with some functions like 'ask' or 'reader'
with types adjusted for the intended usage of the 'Has' class.

-}

module Control.Monad.Reader.Has
( Has(..)
, SuccessfulSearch

, module X
, ask
, asks
, reader
) where

import qualified Control.Monad.Reader as M
import Control.Monad.Reader as X hiding(ask, asks, reader)
import Data.Kind
import Data.Proxy
import GHC.Generics

import Data.Path

type family Search part (g :: k -> Type) :: MaybePath where
  Search part (K1 _ part) = 'Found 'Here
  Search part (K1 _ other) = 'NotFound
  Search part (M1 _ _ x) = Search part x
  Search part (f :*: g) = Combine (Search part f) (Search part g)
  Search _ _ = 'NotFound

class GHas (path :: Path) part grecord where
  gextract :: Proxy path -> grecord p -> part
  gupdate :: Proxy path -> (part -> part) -> grecord p -> grecord p

instance GHas 'Here rec (K1 i rec) where
  gextract _ (K1 x) = x
  gupdate _ f (K1 x) = K1 $ f x

instance GHas path part record => GHas path part (M1 i t record) where
  gextract proxy (M1 x) = gextract proxy x
  gupdate proxy f (M1 x) = M1 (gupdate proxy f x)

instance GHas path part l => GHas ('L path) part (l :*: r) where
  gextract _ (l :*: _) = gextract (Proxy :: Proxy path) l
  gupdate _ f (l :*: r) = gupdate (Proxy :: Proxy path) f l :*: r

instance GHas path part r => GHas ('R path) part (l :*: r) where
  gextract _ (_ :*: r) = gextract (Proxy :: Proxy path) r
  gupdate _ f (l :*: r) = l :*: gupdate (Proxy :: Proxy path) f r

-- | Type alias representing that the search of @part@ in @record@ has been successful.
--
-- The @path@ is used to guide the default generic implementation of 'Has'.
type SuccessfulSearch part record path = (Search part (Rep record) ~ 'Found path, GHas path part (Rep record))

-- | The @Has part record@ class is used for records of type @record@ supporting
-- projecting out a value of type @part@.
class Has part record where
  -- | Extract a subvalue of type @part@ from the @record@.
  --
  -- The default implementation searches for some value of the type @part@ in @record@
  -- and returns that value.
  -- The default implementation typechecks iff there is a single subvalue of type @part@ in @record@.
  extract :: record -> part

  default extract :: forall path. (Generic record, SuccessfulSearch part record path) => record -> part
  extract = gextract (Proxy :: Proxy path) . from

  -- | Update the @record@ given an update function for the @part@.
  --
  -- The default implementation searches for some value of the type @part@ in @record@
  -- and updates that value using the supplied function.
  -- The default implementation typechecks iff there is a single subvalue of type @part@ in @record@.
  update :: (part -> part) -> record -> record

  default update :: forall path. (Generic record, SuccessfulSearch part record path) => (part -> part) -> record -> record
  update f = to . gupdate (Proxy :: Proxy path) f . from

-- | Each type allows projecting itself (and that is an 'id' projection).
instance Has record record where
  extract = id
  update = id

instance SuccessfulSearch a (a0, a1) path => Has a (a0, a1)
instance SuccessfulSearch a (a0, a1, a2) path => Has a (a0, a1, a2)
instance SuccessfulSearch a (a0, a1, a2, a3) path => Has a (a0, a1, a2, a3)
instance SuccessfulSearch a (a0, a1, a2, a3, a4) path => Has a (a0, a1, a2, a3, a4)
instance SuccessfulSearch a (a0, a1, a2, a3, a4, a5) path => Has a (a0, a1, a2, a3, a4, a5)

-- | Retrieves the @part@ of the monad environment.
--
-- This is "Control.Monad.Reader"'s 'Control.Monad.Reader.ask'
-- with the type adjusted for better compatibility with 'Has'.
ask :: (MonadReader record m, Has part record) => m part
ask = M.asks extract

-- | Retrieves a function of the @part@ of the current environment.
--
-- This is "Control.Monad.Reader"'s 'Control.Monad.Reader.asks'
-- with the type adjusted for better compatibility with 'Has'.
asks :: (MonadReader record m, Has part record) => (part -> a) -> m a
asks f = f <$> ask

-- | Retrieves a function of the @part@ of the current environment.
--
-- This is "Control.Monad.Reader"'s 'Control.Monad.Reader.reader'
-- with the type adjusted for better compatibility with 'Has'.
reader :: (MonadReader record m, Has part record) => (part -> a) -> m a
reader = asks
