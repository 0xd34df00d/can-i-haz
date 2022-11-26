{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Path where

data Path = L Path | R Path | Here deriving (Show)
data MaybePath = NotFound | Conflict | Found Path deriving (Show)

type family Combine p1 p2 where
  Combine ('Found path) 'NotFound = 'Found ('L path)
  Combine 'NotFound ('Found path) = 'Found ('R path)
  Combine 'NotFound 'NotFound = 'NotFound
  Combine _ _ = 'Conflict
