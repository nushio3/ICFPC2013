all: devel

.PHONY: deploy devel

devel:
	./dist/build/website/website Development

deploy:
	git pull
	cabal install
	website Production
