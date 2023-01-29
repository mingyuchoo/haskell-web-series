<p align="center">
  <a href="https://github.com/mingyuchoo/haskell-web-series/blob/main/LICENSE"><img alt="license" src="https://img.shields.io/github/license/mingyuchoo/haskell-web-series"/></a>
  <a href="https://github.com/mingyuchoo/haskell-web-series/issues"><img alt="Issues" src="https://img.shields.io/github/issues/mingyuchoo/haskell-web-series?color=appveyor" /></a>
  <a href="https://github.com/mingyuchoo/haskell-web-series/pulls"><img alt="GitHub pull requests" src="https://img.shields.io/github/issues-pr/mingyuchoo/haskell-web-series?color=appveyor" /></a>
</p>

# haskell-web-series

## Basic Cabal Commands

```bash
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

```bash
$ stack new <project>
$ cd <project>

$ stack build
# build more faster
$ stack build --fast --file-watch --ghc-options "-j4 +RTS -A128m -n2m -RTS"

$ stack run
$ stack repl
```
