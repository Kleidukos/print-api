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
