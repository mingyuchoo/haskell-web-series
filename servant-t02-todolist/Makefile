.PHONY: test coverage

all: clean setup build test run

clean:
	stack clean

setup:
	stack setup
	stack test --only-dependencies

build:
	stack build --fast --file-watch --ghc-options "-j4 +RTS -A128m -n2m -RTS"

build-release:
	stack build --docker --ghc-options="-optl-static -fPIC" --flag servant-t02-todolist:static

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
	time stack exec servant-t02-todolist-exe
