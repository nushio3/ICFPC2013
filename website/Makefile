all: build

.PHONY: deploy devel

build:
	cabal build


devel:
	./dist/build/website/website Development

deploy:
	git pull
	cabal build
	./dist/build/website/website Production
