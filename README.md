# can-i-haz

[![Build Status][travis-badge]][travis]
[![Hackage][hackage-badge]][hackage]
[![Stackage LTS][stackage-lts-badge]][stackage-lts]
[![Stackage Nightly][stackage-nightly-badge]][stackage-nightly]

`Generic` implementation of the Has-pattern (mostly useful with `MonadReader`).

## Motivation

Assume there are two types representing the `MonadReader` environments
for different parts of an app:
```haskell
data DbConfig = DbConfig { .. }
data WebConfig = WebConfig { .. }
```
as well as a single type containing both of those:
```haskell
data AppEnv = AppEnv
  { dbConfig :: DbConfig
  , webConfig :: WebConfig
  }
```

What should be the `MonadReader` constraint of the DB module and web module respectively?
1. It could be `MonadReader AppEnv m` for both, introducing unnecessary coupling.
2. Or it could be `MonadReader DbConfig m` for the DB module
   and `MonadReader WebConfig m` for the web module respectively,
   but combining them becomes a pain.

Or, it could be `MonadReader r m, Has DbConfig r` for the DB module,
where `Has` class allows projecting `DbConfig` out of some `r`,
and similarly for the web module!
This approach keeps both modules decoupled, while allowing using them in the same monad stack.

The only downside is that now one has to define the `Has` class
and write tediuos instances for the `AppEnv` type
(and potentially other types in case of tests).

## The solution

This library saves you from this unnecessary boilerplate!
The only thing you have to do is to append the `deriving`-clause:
```haskell
data AppEnv = AppEnv
  { dbConfig :: DbConfig
  , webConfig :: WebConfig
  } deriving (Generic, Has DbConfig, Has WebConfig)
```
and use `ask extract` instead of `ask` (but this is something you'd have to do anyway).

## Documentation

Perhaps the best source is the [Haddock docs](http://hackage.haskell.org/package/can-i-haz/docs/Control-Monad-Reader-Has.html).

## Acknowledgements

Thanks lyxia @ #haskell for the type families-based derivation of the `GHas` instance.

[travis]:        <https://travis-ci.org/0xd34df00d/can-i-haz>
[travis-badge]:  <https://travis-ci.org/0xd34df00d/can-i-haz.svg?branch=master>
[hackage]:       <https://hackage.haskell.org/package/can-i-haz>
[hackage-badge]: <https://img.shields.io/hackage/v/can-i-haz.svg>
[stackage-lts-badge]: <http://stackage.org/package/can-i-haz/badge/lts>
[stackage-nightly-badge]: <http://stackage.org/package/can-i-haz/badge/nightly>
[stackage-lts]: <http://stackage.org/lts/package/can-i-haz>
[stackage-nightly]: <http://stackage.org/nightly/package/can-i-haz>
