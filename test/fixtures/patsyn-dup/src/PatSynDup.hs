-- Reproducer for a duplicate export that should be
-- de-duplicated by print-api.
{-# LANGUAGE PatternSynonyms #-}

module PatSynDup
  ( Foo (.., Bar)
  , pattern Bar
  ) where

data Foo = MkFoo Int

pattern Bar :: Int -> Foo
pattern Bar n = MkFoo n
