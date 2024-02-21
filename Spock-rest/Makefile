all: clean setup build test run

clean:
	stack clean

setup:
	stack setup
	stack test --only-dependencies

build:
	stack build

test:
	stack test

run:
	stack run
