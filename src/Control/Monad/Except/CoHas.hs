{-# LANGUAGE TypeOperators, DataKinds, PolyKinds, TypeFamilies, ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables, DefaultSignatures #-}
{-# LANGUAGE Safe #-}

{-|
Description: Generic implementation of the CoHas injection pattern (dual to Has)
Stability: experimental

This module defines a class 'CoHas' intended to be used with the 'Control.Monad.Except.MonadError' class
(and similar ones) or 'Control.Monad.Except.Except'/'Control.Monad.ExceptT' types.

= The problem

Assume there are several types representing the possible errors in different parts of an application:

@
data DbError = ...
data WebUIError = ...
@

as well as a single sum type containing all of those:

@
data AppError
  = AppDbError DbError
  | AppWebUIError WebUIError
@

What should be the @MonadError@ constraint of the DB module and web module respectively?

1. It could be @MonadError AppError m@ for both, introducing unnecessary coupling.

2. Or it could be @MonadError DbError m@ for the DB module and
   @MonadError WebError m@ for the web module respectively, but combining them becomes a pain.

Or, it could be @MonadError e m, CoHas AppError e@ for the DB module (and similarly for the web module),
where some appropriately defined @CoHas option sum@ class allows injecting @option@
creating a value of the @sum@ type.
This approach keeps both modules decoupled, while allowing using them in the same monad stack.

The only downside is that now one has to define the @CoHas@ class
and write tedious instances for the @AppError@ type (and potentially other types in case of, for example, tests).

But why bother doing the work that the machine will happily do for you?

= The solution

This module defines the generic 'CoHas' class as well as hides all the boilerplate behind "GHC.Generics",
so all you have to do is to add the corresponding @deriving@-clause:

@
data AppError
  = AppDbError DbError
  | AppWebUIError WebUIError
  deriving (Generic, CoHas DbError, CoHas WebUIError)
@

and use @throwError . inject@ instead of @throwError@ (but this is something you'd have to do anyway).

= Type safety

What should happen if @sum@ does not have any way to construct it from @option@ at all?
Of course, this means that we cannot inject @option@ into @sum@, and no 'CoHas' instance can be derived at all.
Indeed, this library will refuse to generate an instance in this case.

On the other hand, what should happen if @sum@ contains multiple values of type @option@
(like @Either option option@), perhaps on different levels of nesting?
While technically we could make an arbitrary choice, like taking the first one in breadth-first or depth-first order,
we instead decide that such a choice is inherently ambiguous,
so this library will refuse to generate an instance in this case as well.

-}

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
