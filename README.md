# dump-decls

Taken from the GHC CI scripts, this tool prints a human-readable representation of the exposed interface of Haskell modules that you
can use in CI to ensure that you do not mistakenly introduce a regression.

## Usage

Go in your project and build it with the GHC environment files enabled:

```
$ cabal build --write-ghc-environment-files=always
```

Then run the `dump-decls` binary from within the same directory:

```
$ dump-decls -p <my-package>
```

For instance in the [`text-display`](https://github.com/haskell-text/text-display) repository:

```haskell
❯ dump-decls -p text-display

module Data.Text.Display where
  type Display :: * -> Constraint
  class Display a where
    displayBuilder :: a -> Data.Text.Internal.Builder.Builder
    displayList :: [a] -> Data.Text.Internal.Builder.Builder
    displayPrec :: GHC.Types.Int -> a -> Data.Text.Internal.Builder.Builder
    {-# MINIMAL displayBuilder | displayPrec #-}
    {-# MINIMAL displayBuilder | displayPrec #-}
  type role OpaqueInstance phantom representational
```
