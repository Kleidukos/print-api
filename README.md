# print-api

_(If you want to use this tool as a GitHub Workflow, check out [diff-package-api](https://github.com/Kleidukos/diff-package-api/))_

Taken from the GHC CI scripts, this tool prints a human-readable representation of the exposed interface of Haskell modules that you
can use in CI to ensure that you do not mistakenly introduce a regression.

The tool can be used with the following GHC versions:

* 9.10.1
* 9.8.2
* 9.6.6

[Releases](https://github.com/Kleidukos/print-api/releases/latest) provide binaries for the following operating systems:

* Linux x86_64 (glibc, dynamic)
* Linux x86_64 (musl, static)
* macOS arm64

Pick the one that matches your operating system *and* GHC version.

## Usage

Go in your project and build it with the GHC environment files enabled:

```
$ cabal build --write-ghc-environment-files=always
```

Then run the `print-api` binary from within the same directory:

```
$ print-api -p <my-package>
```

For instance in the [`text-display`](https://github.com/haskell-text/text-display) repository:

```haskell
❯ print-api -p text-display

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
