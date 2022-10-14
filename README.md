# haskell-web-series

## Basic Cabal Commands

```sh
$ mkdir <project>
$ cd <project>
$ cabal init

...

$ cabal install —only-dependencies
$ cabal update
$ cabal configure
$ cabal check
$ cabal build
$ cabal run
$ cabal sdist
$ cabal upload
$ cabal install

```

### Basic Stack Commands

```sh
$ stack new <project>
$ cd <project>

$ stack build
# build more faster
$ stack build --fast --file-watch --ghc-options "-j4 +RTS -A128m -n2m -RTS"

$ stack run
$ stack repl
```
