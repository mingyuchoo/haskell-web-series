# warp-t01-json

## How to create a project

```bash
stack new <project-name> mingyuchoo/new-template
```

## How to build

```bash
stack build
# or
stack build --fast --file-watch --ghc-options "-j4 +RTS -A128m -n2m -RTS"
```

## How to test as watch mode

```bash
stack test --fast --file-watch --watch-all
# or
stack test --coverage --fast --file-watch --watch-all --haddock
# or
ghcid --command "stack ghci test/Spec.hs"
```

## How to run

```bash
stack run
```

You can also use `Makefile` for these works.

## How to request

### GET method

1. Open web browser
2. Call `http://localhost:4000/expr?q=HelloWorld!`
3. You can find `HelloWorld!` in your web browser

### POST method

`curl -X POST http://localhost:4000/`

### PUT method

`curl -X PUT http://localhost:4000/`

### DELETE method

`curl -X DELETE http://localhost:4000/`

## References

- <https://aosabook.org/en/posa/warp.html>
- <https://crypto.stanford.edu/~blynn/haskell/warp.html>
- <https://stackoverflow.com/questions/22620294/minimal-warp-webserver-example>
- <https://wiki.haskell.org/Web/Servers#Warp>
