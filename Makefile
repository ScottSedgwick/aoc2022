build: $(wildcard src/*.hs) app/Main.hs aoc2022.cabal  	## Build the application
	cabal build --enable-tests

run: build												## Build and Run the application
	cabal exec aoc2022

test: $(wildcard test/**/*.hs)							## Build and Run the Test Suite
	cabal test

.DEFAULT_GOAL := help
.PHONY: help

help:
	@grep -E '^[a-zA-Z0-9_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
	| sed -n 's/^\(.*\): \(.*\)##\(.*\)/\1\3/p' \
	| column -t  -s ' '