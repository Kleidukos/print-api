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

If you have the GitHub CLI installed, (something like) the following does work:

```
$ gh release download --repo Kleidukos/print-api --pattern "print-api-*-$(uname -s)-$(uname -m).tar.gz" --output - | tar -x -z -C <INSTALL_DIRECTORY>
```

## Usage

Go in your project and build it with the GHC environment files enabled:

```
$ cabal build --write-ghc-environment-files=always
```

You need the appropriate `print-api-<GHC_VERSION>` in your `PATH`. That means,
if you did not obtain the executables from the GitHub Releases as described
above but build and installed `print-api` yourself, then you need to install it
for the GHC version you intend to use; For example:

```
$ git clone https://github.com/Kleidukos/print-api.git
$ cd print-api
$ cabal install --with-compiler ghc-<GHC_VERSION>
```

(There is also an `install-for-ghcup-compilers.sh` script in the `print-api`
repository that installs `print-api` for GHCs currently installed by GHCup.)

Then run the `print-api` binary from within the source tree of your project:

```
$ print-api -p <my-package>
```

### Ignore list of modules

By passing the `--modules-ignore-list FILE` option to `print-api`, you can specify
a list of newline-delimited module names that will be ignored by `print-api`,
and shown as having been ignored in the output.

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
