#!/bin/bash
# One line explanation of deploy-solver-servers.sh.
./mapssh "$1" "$2" 'cd ICFPC2013; git pull; crontab crontab.conf; cd solver; cabal install; killall solver-api-server'
