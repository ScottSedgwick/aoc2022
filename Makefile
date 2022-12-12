build:		 				## Build the application
	cabal build

run: build					## Build and Run the application
	cabal exec aoc2022

test: build					## Build and Run the Test Suite
	cabal test

.DEFAULT_GOAL := help
.PHONY: help

help:
	@grep -E '^[a-zA-Z0-9_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
	| sed -n 's/^\(.*\): \(.*\)##\(.*\)/\1\3/p' \
	| column -t  -s ' '