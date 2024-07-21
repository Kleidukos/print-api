## NAME

print-api – Export the public API of your Haskell package

## SYNOPSIS

<dl>
    <dt>print-api <i>(-p|--package-name PACKAGE NAME)</i></dt>
    <dd style="margin-left: 1rem;">Export the API of a package</dd>
</dl>

## DESCRIPTION

*print-api* exports the API exposed by a Haskell package. This is used to maintain a file that can be tracked by version control and used in Golden Tests.

## OPTIONS

### -p|--package-name PACKAGE NAME

_Mandatory_

Specify for which package you wish to have the API export. 

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
