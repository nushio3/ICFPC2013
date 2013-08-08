all: build

.PHONY: deploy devel

init:
	cabal install --only-dependencies --enable-tests
	cabal configure --enable-tests

build:
	cabal build


devel:
	./dist/build/website/website Development

deploy:
	git pull
	cabal build
	./dist/build/website/website Production
