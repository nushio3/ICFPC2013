all: deploy

.PHONY: deploy

deploy:
	git pull
	cabal install
	website Production
