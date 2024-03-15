.PHONY: test coverage

all: clean setup build test run

clean:
	stack clean

setup:
	stack setup
	stack test --only-dependencies

build:
	stack build

build-fast:
	stack build --fast --file-watch --ghc-options "-j4 +RTS -A128m -n2m -RTS"

test:
	stack test --fast

coverage:
	stack test --coverage --fast --haddock

watch-test:
	stack test --fast --file-watch --watch-all

watch-coverage:
	stack test --coverage --fast --file-watch --watch-all --haddock

ghcid:
	ghcid --command "stack ghci test/Spec.hs"

run:
	time stack exec socket-echo-server-exe
