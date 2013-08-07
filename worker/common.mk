.PHONY: init build clean doc sync test CI

init:
	cabal install --only-dependencies --enable-tests
	cabal configure --enable-tests

build: init
	cp ../website/Contents/Salt.hs exe-src/Contents/
	cabal build

clean:
	cabal clean

doc:
	cabal haddock --hyperlink-source

test: build
	cabal test

install: init build test
	cabal install

sync:
	git pull

CI: sync test
